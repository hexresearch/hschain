{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- import Codec.Serialise (Serialise)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Int
import Data.Word
import qualified Data.Map             as Map
import qualified Data.ByteString.Lazy as BS
import           Data.Map (Map)
import System.IO

import Thundermint.Blockchain.App
import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.P2P
import Thundermint.Crypto

-- Mock crypto which works as long as no one tries to break it.
data Swear

newtype instance PrivKey   Swear = SwearPrivK Word8
newtype instance PublicKey Swear = SwearPubK Word8
-- newtype instance Signature Swear = SwearSig ()

-- | We assume that there's
instance Crypto Swear where
  signBlob            _ _   = Signature ""
  verifyBlobSignature _ _ _ = True
  publicKey (SwearPrivK w)  = SwearPubK w
  address   (SwearPubK  w)  = Address $ BS.pack [w]
  hashBlob                  = Hash

----------------------------------------------------------------
--
----------------------------------------------------------------

startNode
  :: ()
  => Blockchain Swear Int64
  -> Map (Address Swear) (Validator Swear)
  -> PrivValidator Swear Int64
  -> IO (AppChans Swear Int64, Async ())
startNode blockchain vals privValidator = do
  appCh     <- newAppChans
  blockchTV <- newTVarIO blockchain
  store     <- newTVarIO Map.empty
  -- Thread with main application
  file <- openFile ("logs/" ++ let SwearPrivK nm = validatorPrivKey privValidator
                               in show nm
                   ) WriteMode
  let logger s = do hPutStrLn file s
                    hFlush file
  let appState = AppState { appBlockchain    = blockchTV
                          , appBlockStore    = store
                          , appProposalMaker = \r commit -> do
                              bch <- readTVar blockchTV
                              let lastBlock = case bch of
                                    Cons    b _ -> b
                                    Genesis b   -> b
                              let Height h = headerHeight $ blockHeader lastBlock
                                  block = Block
                                    { blockHeader     = Header
                                        { headerChainID     = "TEST"
                                        , headerHeight      = Height (h + 1)
                                        , headerTime        = Time 0
                                        , headerLastBlockID = Just (blockHash lastBlock)
                                        }
                                    , blockData       = h * 100
                                    , blockLastCommit = commit
                                    }
                              return $ Proposal { propHeight  = Height (h + 1)
                                                , propRound   = r
                                                , propTimestamp = Time 0
                                                , propPOL     = Nothing
                                                , propBlockID = blockHash block
                                                , propBlock   = block
                                                }
                          , appValidator     = privValidator
                          , appValidatorsSet = vals
                          , appLogger        = logger
                          , appMaxHeight     = Just (Height 3)
                          }
  hnd <- async $ runApplication appState appCh
  --
  return (appCh, hnd)


----------------------------------------------------------------
--
----------------------------------------------------------------

validators :: [PrivValidator Swear Int64]
validators =
  [ PrivValidator { validatorPrivKey  = SwearPrivK 1
                  , validateBlockData = undefined
                  }
  , PrivValidator { validatorPrivKey  = SwearPrivK 2
                  , validateBlockData = undefined
                  }
  , PrivValidator { validatorPrivKey  = SwearPrivK 3
                  , validateBlockData = undefined
                  }
  , PrivValidator { validatorPrivKey  = SwearPrivK 4
                  , validateBlockData = undefined
                  }
  ]

validatorSet :: Map (Address Swear) (Validator Swear)
validatorSet = Map.fromList
  [ ( address (publicKey (validatorPrivKey v))
    , Validator { validatorPubKey      = publicKey (validatorPrivKey v)
                , validatorVotingPower = 1
                }
    )
  | v <- validators
  ]

genesisBlock :: Blockchain Swear Int64
genesisBlock = Genesis Block
  { blockHeader = Header
      { headerChainID     = "TEST"
      , headerHeight      = Height 0
      , headerTime        = Time 0
      , headerLastBlockID = Nothing
      }
  , blockData       = 0
  , blockLastCommit = Nothing
  }

main :: IO ()
main = do
  -- Start application for all validators
  appChans <- mapM (startNode genesisBlock validatorSet) validators
  -- Connect each application to each other
  let pairs = [ (ach1, ach2)
              | (i,(ach1,_)) <- zip [1::Int ..] appChans
              , (j,(ach2,_)) <- zip [1::Int ..] appChans
              , i > j
              ]
  forM_ pairs $ \(chA, chB) -> do
    chA2B <- newTChanIO
    chB2A <- newTChanIO
    txA   <- atomically $ dupTChan $ appChanTx chA
    txB   <- atomically $ dupTChan $ appChanTx chB
    startPeer
      chA { appChanTx = txA }
      Connection { sendEnd = atomically . writeTChan chA2B
                 , recvEnd = readTChan chB2A
                 }
    startPeer
      chB { appChanTx = txB }
      Connection { sendEnd = atomically . writeTChan chB2A
                 , recvEnd = readTChan chA2B
                 }
  -- Wait until done.
  forM_ appChans $ \(_,a) -> wait a
