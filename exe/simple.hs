{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Codec.Serialise (Serialise)
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Crypto.Hash.MD5 as MD5
import Data.Int
import Data.Word
import qualified Data.ByteString      as BS
import           Data.Map               (Map)
import qualified Data.Map             as Map
import System.IO
import Data.Time.Clock (getCurrentTime,diffUTCTime,UTCTime)
import Text.Printf

import Thundermint.Blockchain.App
import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.P2P
import Thundermint.Crypto
import Thundermint.Store

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
  hashBlob                  = Hash . MD5.hashlazy

----------------------------------------------------------------
--
----------------------------------------------------------------

newSTMBlockStorage
  :: (Crypto alg, Serialise a)
  => Block alg a
  -> IO (BlockStorage 'RW IO alg a)
newSTMBlockStorage gBlock = do
  -- FIXME: we MUST require correct genesis block
  varBlocks <- newTVarIO $ Map.singleton (Height 0) gBlock
  varPBlk   <- newTVarIO $ Map.empty
  varLCmt   <- newTVarIO Nothing
  let currentHeight = do
        Just (h,_) <- Map.lookupMax <$> readTVar varBlocks
        return h
  return BlockStorage
    { blockchainHeight = atomically currentHeight
    , retrieveBlock    = \h -> do m <- readTVarIO varBlocks
                                  return $ Map.lookup h m
    , retrieveLastCommit = readTVarIO varLCmt
    , storeCommit = \cmt blk -> atomically $ do
        h <- currentHeight
        modifyTVar' varBlocks $ Map.insert (next h) blk
        writeTVar   varLCmt (Just cmt)
        writeTVar   varPBlk Map.empty

    , retrievePropBlocks = \height -> atomically $ do
        h <- currentHeight
        if h == height then readTVar varPBlk
                       else return Map.empty
    , storePropBlock = \height blk -> atomically $ do
        h <- currentHeight
        when (height == h) $ do
          let bid = blockHash blk
          modifyTVar varPBlk $ Map.insert bid blk
    }


----------------------------------------------------------------
--
----------------------------------------------------------------

startNode
  :: ()
  => UTCTime
  -- -> Blockchain Swear Int64
  -> Map (Address Swear) (Validator Swear)
  -> PrivValidator Swear Int64
  -> IO (AppChans Swear Int64, Async ())
startNode t0 vals privValidator = do
  appCh     <- newAppChans
  appSt     <- newSTMBlockStorage genesisBlock
  -- Thread with main application
  file <- openFile ("logs/" ++ let SwearPrivK nm = validatorPrivKey privValidator
                               in show nm
                   ) WriteMode
  let logger s = do t <- getCurrentTime
                    hPutStr   file
                      (printf "%10.3f: " (realToFrac (diffUTCTime t t0) :: Double))
                    hPutStrLn file s
                    hFlush    file
  let appState = AppState { appStorage = appSt
                          , appBlockGenerator = \commit -> do
                              -- FIXME: We need to fetch last block
                              Just lastBlock <- retrieveBlock appSt =<< blockchainHeight appSt
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
                              return block
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

genesisBlock :: Block Swear Int64
genesisBlock = Block
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
  t0 <- getCurrentTime
  appChans <- mapM (startNode t0 validatorSet) validators
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
