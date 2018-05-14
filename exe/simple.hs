{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

import Codec.Serialise (Serialise)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import qualified Crypto.Hash.MD5 as MD5
import Data.Int
import Data.Word
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as BC8
import qualified Data.ByteString        as BS
import           Data.Map               (Map)
import qualified Data.Map             as Map
import System.IO
import Data.Time.Clock (getCurrentTime,diffUTCTime,UTCTime)
import Text.Printf
import qualified Katip

import Thundermint.Blockchain.App
import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.P2P
import Thundermint.P2P.Network
import Thundermint.Crypto
import Thundermint.Store
import Thundermint.Logger


----------------------------------------------------------------
--
----------------------------------------------------------------

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
  let bs = BlockStorage
        { blockchainHeight = atomically currentHeight
        , retrieveBlock    = \h -> do m <- readTVarIO varBlocks
                                      return $ Map.lookup h m
        , retrieveBlockID  = (fmap . fmap) blockHash . retrieveBlock bs
        , retrieveCommit   = error "No implementation for retrieveCommit"
        , retrieveLastCommit = readTVarIO varLCmt
        , storeCommit = \cmt blk -> atomically $ do
            h <- currentHeight
            modifyTVar' varBlocks $ Map.insert (next h) blk
            writeTVar   varLCmt (Just cmt)
            writeTVar   varPBlk Map.empty
        --
        , retrievePropBlocks = \height -> atomically $ do
            h <- currentHeight
            if h == height then readTVar varPBlk
                           else return Map.empty
        , retrieveStoredProps = atomically $ do
            h  <- currentHeight
            bs <- readTVar varPBlk
            return (h, Map.keysSet bs)
        , storePropBlock = \height blk -> atomically $ do
            h <- currentHeight
            when (height == h) $ do
              let bid = blockHash blk
              modifyTVar varPBlk $ Map.insert bid blk
        }
  return bs


-- ----------------------------------------------------------------
-- --
-- ----------------------------------------------------------------

-- startNode
--   :: ()
--   => UTCTime
--   -- -> Blockchain Swear Int64
--   -> Map (Address Swear) (Validator Swear)
--   -> PrivValidator Swear Int64
--   -> IO (AppChans Swear Int64, Async ())
-- startNode t0 vals privValidator = do
--   appCh     <- newAppChans
--   appSt     <- newSTMBlockStorage genesisBlock
--   -- Thread with main application
--   file <- openFile ("logs/" ++ let SwearPrivK nm = validatorPrivKey privValidator
--                                in show nm
--                    ) WriteMode
--   let logger s = do t <- getCurrentTime
--                     hPutStr   file
--                       (printf "%10.3f: " (realToFrac (diffUTCTime t t0) :: Double))
--                     hPutStrLn file s
--                     hFlush    file
--   hnd <- async $ runApplication appState appCh
--   --
--   return (appCh, hnd)


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

----------------------------------------------------------------
--
----------------------------------------------------------------

-- Start node which will now run consensus algorithm
startNode
  :: (Ord addr, Show addr, Crypto alg, Serialise a, a ~ Int64)
  => NetworkAPI sock addr
  -> [addr]
  -> PrivValidator alg a
  -> Map (Address alg) (Validator alg)
  -> Block alg a
  -> IO ()
startNode net addrs val valSet genesis = do
  -- Initialize logging
  scribe <- Katip.mkFileScribe
    ("logs/" ++ let Address nm = address $ publicKey $ validatorPrivKey val
                in BC8.unpack (Base16.encode nm)
    ) Katip.DebugS Katip.V2
  logenv <- Katip.registerScribe "log" scribe Katip.defaultScribeSettings
        =<< Katip.initLogEnv "TM" "DEV"
  flip finally (Katip.closeScribes logenv) $ do
    -- Initialize block storage
    storage <- newSTMBlockStorage genesis
    appCh   <- newAppChans
    --
    let appState = AppState { appStorage        = hoistBlockStorageRW liftIO storage
                            , appBlockGenerator = \commit -> liftIO $ do
                                -- FIXME: We need to fetch last block
                                Just lastBlock <- retrieveBlock storage =<< blockchainHeight storage
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
                            , appValidator     = val
                            , appValidatorsSet = valSet
                            , appMaxHeight     = Just (Height 3)
                            }
    -- Start P2P
    let netRoutine = runLoggerT "net" logenv
                   $ startPeerDispatcher net addrs appCh (makeReadOnly storage)
    withAsync netRoutine $ \_ -> do
      runLoggerT "consensus" logenv $ runApplication appState appCh


withAsyncs :: [IO a] -> ([Async a] -> IO b) -> IO b
withAsyncs ios function
  = recur ([],ios)
  where
    recur (as,[])   = function (reverse as)
    recur (as,i:is) = withAsync i $ \a -> recur (a:as, is)

main :: IO ()
main = do
  net <- newMockNet
  let actions = [ do let node  = createMockNode net addr
                         addrs = [ (a,"50000") | (a, _) <- [1::Int ..] `zip` validators
                                               , a < addr
                                               ]
                     startNode node addrs val validatorSet genesisBlock
                | (addr, val) <- [1::Int ..] `zip` validators
                ]
  withAsyncs actions $ mapM_ wait
