{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

import Codec.Serialise          (Serialise)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Int
import Data.Map                 (Map)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as BC8
import qualified Data.Map               as Map
import qualified Katip

import Thundermint.Blockchain.App
import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Crypto.Ed25519   (Ed25519_SHA512, privateKey)
import Thundermint.Logger
import Thundermint.P2P
import Thundermint.P2P.Network
import Thundermint.Store


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
        , retrieveCommit   = \h -> atomically $ do
            hMax <- currentHeight
            if h == hMax then readTVar varLCmt
                         else do bmap <- readTVar varBlocks
                                 return $ blockLastCommit =<< Map.lookup (next h) bmap
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
            blk <- readTVar varPBlk
            return (h, Map.keysSet blk)
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

{-
-}
-- FIXME: replace base16 with base58

fromBase16 :: BS.ByteString -> BS.ByteString
fromBase16 = fst . Base16.decode

-- FIXME: keys show be stored somewhere

validators :: Map BS.ByteString  (PrivValidator Ed25519_SHA512 Int64)
validators = Map.fromList
  [ n .= PrivValidator { validatorPrivKey  = privateKey $ fromBase16 n
                       , validateBlockData = const True
                       }
  | n <- [ "137f97f2a73e576b8b8d52b3728088ac6c25383065853b5d049da74100f6a2db"
         , "32111ba438148948ab3119f4f2132530ec844de1bf2521fa840555a9afcf15dd"
         , "217d248f6623d335692d6198a6121ae24e6dac97c1e70ea60e0ce4ee2099d7b5"
         , "b2b9b660e40438ed7a4e3d05b108eec78c4915f91982bb480d6a06109a01f7de"
         , "4f1c6d2b0a704e2ac8803cb53bb6b03b08a48557ee705f20027b861636353075"
         , "5e00991c969498a3948c67e614041f5e47591fac761e10ba84bde69b1189badb"
         , "66650c20d918afaa0a353b32613ffa63d56da2f6ad67757df32d7dec58b143dd"
         ]
  ]
  where (.=) = (,)

genesisBlock :: Block Ed25519_SHA512 Int64
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

makeValidatorSet
  :: (Foldable f, Crypto alg)
  => f (PrivValidator alg a) -> Map (Address alg) (Validator alg)
makeValidatorSet vals = Map.fromList
  [ ( address (publicKey (validatorPrivKey v))
    , Validator { validatorPubKey      = publicKey (validatorPrivKey v)
                , validatorVotingPower = 1
                }
    )
  | v <- toList vals
  ]

connectAll2All :: Ord addr => Map addr a -> addr -> [addr]
connectAll2All vals addr =
  [ a
  | a <- Map.keys vals
  , a < addr
  ]

connectRing :: Ord addr => Map addr a -> addr -> [addr]
connectRing vals addr =
  case Map.splitLookup addr vals of
    (_ , Nothing, _ ) -> []
    (va, Just _ , vb) -> case Map.lookupMin vb of
      Just (a,_) -> [a]
      Nothing    -> case Map.lookupMin va of
        Just (a,_) -> [a]
        Nothing    -> []


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
  let validatorSet = makeValidatorSet validators
      actions = [ do let node  = createMockNode net addr
                         addrs = map (,"50000") $ connectRing validators addr
                     startNode node addrs val validatorSet genesisBlock
                | (addr, val) <- Map.toList validators
                ]
  withAsyncs actions $ mapM_ wait
