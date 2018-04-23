{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Word
import qualified Data.Map        as Map
import qualified Data.ByteString as BS
import           Data.Map (Map)

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
  hashValue = error "hashValue:Swear"

----------------------------------------------------------------
--
----------------------------------------------------------------

startNode
  :: Blockchain alg a
  -> Map (Address alg) (Validator alg)
  -> PrivValidator alg a
  -> IO (AppChans alg a)
startNode blockchain validators privValidator = do
  appCh     <- newAppChans
  blockchTV <- newTVarIO blockchain
  commitTV  <- newTVarIO Nothing
  -- Thread with main application
  let appState = AppState { appBlockchain    = blockchTV
                          , appLastCommit    = commitTV
                          -- , appProposalMaker = 
                          , appValidator     = privValidator
                          , appValidatorsSet = validators
                          }
  --
  return appCh
  

----------------------------------------------------------------
--
----------------------------------------------------------------

validators :: [PrivValidator Swear Int]
validators =
  [ PrivValidator { validatorPrivKey = SwearPrivK 1
                  , validateBlockData = undefined
                  }
  , PrivValidator { validatorPrivKey = SwearPrivK 2
                  , validateBlockData = undefined
                  }
  , PrivValidator { validatorPrivKey = SwearPrivK 3
                  , validateBlockData = undefined
                  }
  , PrivValidator { validatorPrivKey = SwearPrivK 4
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

genesisBlock :: Blockchain Swear Int
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
              | (i,ach1) <- zip [1..] appChans
              , (j,ach2) <- zip [1..] appChans
              , i > j
              ]
  forM_ pairs $ \(chA, chB) -> do
    chA2B <- newTChanIO
    chB2A <- newTChanIO    
    startPeer chA Connection { sendEnd = atomically . writeTChan chA2B
                             , recvEnd = readTChan chB2A
                             }
    startPeer chB Connection { sendEnd = atomically . writeTChan chB2A
                             , recvEnd = readTChan chA2B
                             }
  -- Wait until done.
  return ()
