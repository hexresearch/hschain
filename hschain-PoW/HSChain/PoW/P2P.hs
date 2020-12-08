{-# LANGUAGE KindSignatures #-}
-- |
module HSChain.PoW.P2P
  ( -- * Full node
    PoW(..)
  , MempoolAPI(..)
  , startNode
  , startNodeTest
    -- * Light node
  , LightConsensus(..)
  , LightPoW(..)
  , lightNode
  ) where

import Codec.Serialise
import Control.Applicative
import Control.Lens
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.Cont
import Control.Monad.Catch

import HSChain.Control.Channels
import HSChain.Control.Class
import HSChain.Logger
import HSChain.Network.Types
import HSChain.PoW.Consensus
import HSChain.PoW.Mempool
import HSChain.PoW.P2P.Handler.BlockRequests
import HSChain.PoW.P2P.Handler.Consensus
import HSChain.PoW.P2P.Handler.PEX
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- Full node
----------------------------------------------------------------

-- | Dictionary with functions for interacting with consensus engine
data PoW view = PoW
  { currentConsensus :: STM (Consensus view)
    -- ^ View on current state of consensus (just a read from TVar).
  , sendNewBlock     :: BlockOf view -> MonadOf view (Either SomeException ())
    -- ^ Send freshly mined block to consensus
  , chainUpdate      :: STM (Src view)
    -- ^ Create new broadcast source which will recieve message every
    --   time head is changed
  , mempoolAPI       :: MempoolAPI view
    -- ^ API for communication with mempool
  }


startNode
  :: ( MonadMask m, MonadFork m, MonadLogger m
     , Serialise (b Identity)
     , Serialise (b Proxy)
     , Serialise (Tx b)
     , StateView' view m b
     )
  => NodeCfg
  -> NetworkAPI
  -> BlockDB   m b
  -> Consensus view
  -> ContT r m (PoW view)
startNode cfg netAPI db consensus
  = fst <$> startNodeTest cfg netAPI db consensus

-- | Same as startNode but expose internal interfacees for testing
startNodeTest
  :: ( MonadMask m, MonadFork m, MonadLogger m
     , Serialise (b Identity)
     , Serialise (b Proxy)
     , Serialise (Tx b)
     , StateView' view m b
     )
  => NodeCfg
  -> NetworkAPI
  -> BlockDB   m b
  -> Consensus view
  -> ContT r m ( PoW view
               , Sink (BoxRX m b)
               )
startNodeTest cfg netAPI db consensus = do
  lift $ logger InfoS "Starting PoW node" ()
  (sinkBOX,    srcBOX)     <- queuePair
  (sinkAnn,    mkSrcAnn)   <- broadcastPair
  (sinkChain,  mkSrcChain) <- broadcastPair
  (sinkBIDs,   srcBIDs)    <- queuePair
  blockReg                 <- newBlockRegistry srcBIDs
  bConsesus                <- liftIO $ newTVarIO consensus
  -- Start mempool
  (mempoolAPI,MempoolCh{..}) <- startMempool db (consensus ^. bestHead . _1)
  -- Start PEX
  let pexCh = PexCh
        { pexNodeCfg        = cfg
        , pexNetAPI         = netAPI
        , pexSinkTX         = postTransaction mempoolAPI
        , pexMkAnnounce     = liftA2 (<>)
            (fmap GossipAnn <$> mkSrcAnn)
            (fmap GossipTX  <$> mempoolAnnounces)
        , pexSinkBox        = sinkBOX
        , pexBlockRegistry  = blockReg
        , pexConsesusState  = do c <- readTVar bConsesus
                                 pure ( c ^. blockIndex
                                      , c ^. bestHead . _1 . to stateBH
                                      , c ^. bestHead . _2)
        }
  runPEX pexCh db
  -- Consensus thread
  let consensusCh = ConsensusCh
        { bcastAnnounce    = sinkAnn
        , bcastChainUpdate = sinkChain
                          <> (contramap (\(bh,s) -> MempHeadChange bh s) mempoolConsensusCh)
        , sinkConsensusSt  = Sink $ writeTVar bConsesus
        , sinkReqBlocks    = sinkBIDs
        , srcRX            = srcBOX
        }
  cforkLinked $ threadConsensus db consensus consensusCh
  return
    ( PoW { currentConsensus     = readTVar bConsesus
          , sendNewBlock         = \(!b) -> do
              res <- liftIO newEmptyMVar
              sinkIO sinkBOX $ BoxRX $ \cnt -> liftIO . putMVar res =<< cnt (RxMined b)
              liftIO (takeMVar res) >>= \case
                Peer'Punish e     -> return (Left e)
                Peer'EnterCatchup -> return (Right ())
                Peer'Noop         -> return (Right ())
          , chainUpdate          = fmap (\(_,s) -> s) <$> mkSrcChain
          , ..
          }
    , sinkBOX
    )


----------------------------------------------------------------
-- Light node
----------------------------------------------------------------

data LightPoW b = LightPoW
  { bestHeadUpdates :: STM (Src (LightConsensus b))
  }

lightNode
  :: ( MonadMask m, MonadFork m, MonadLogger m
     , Serialise (b Identity)
     , Serialise (b Proxy)
     , Serialise (Tx b)
     , BlockData b
     )
  => NodeCfg
  -> NetworkAPI
  -> BlockDB   m b
  -> LightConsensus b
  -> ContT r m (LightPoW b)
lightNode cfg netAPI db consensus = do
  lift $ logger InfoS "Starting PoW node" ()
  (sinkBOX, srcBOX)   <- queuePair
  (sinkUpd, mkSrcUpd) <- broadcastPair
  (sinkAnn, mkSrcAnn) <- broadcastPair
  blockReg            <- newBlockRegistry mempty
  vConsensus          <- liftIO $ newTVarIO consensus
  -- Start PEX
  let pexCh = PexCh
        { pexNodeCfg       = cfg
        , pexNetAPI        = netAPI
        , pexSinkTX        = mempty
        , pexSinkBox       = sinkBOX
        , pexMkAnnounce    = fmap GossipAnn <$> mkSrcAnn
        , pexBlockRegistry = blockReg
        , pexConsesusState = do c <- readTVar vConsensus
                                pure ( c ^. lightBlockIndex
                                     , c ^. bestLightHead . _1
                                     , c ^. bestLightHead . _2
                                     )
        }
  runPEX pexCh db
  --
  let lightCh = LightConsensusCh
        { lcUpdate   = sinkUpd
                    <> Sink (writeTVar vConsensus)
        , lcAnnounce = sinkAnn
        , lcRX       = srcBOX
        }
  cforkLinked $ threadLightConsensus db undefined lightCh
  -- Done
  pure LightPoW { bestHeadUpdates = mkSrcUpd }
