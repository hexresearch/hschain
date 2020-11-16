{-# LANGUAGE KindSignatures #-}
-- |
module HSChain.PoW.P2P
  ( PoW(..)
  , MempoolAPI(..)
  , startNode
  , startNodeTest
  ) where

import Codec.Serialise
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


-- | Dictionary with functions for interacting with consensus engine
data PoW view m b = PoW
  { currentConsensus :: STM (Consensus view m b)
    -- ^ View on current state of consensus (just a read from TVar).
  , sendNewBlock     :: Block b -> m (Either SomeException ())
    -- ^ Send freshly mined block to consensus
  , chainUpdate      :: STM (Src (BH b, view))
    -- ^ Create new broadcast source which will recieve message every
    --   time head is changed
  , mempoolAPI       :: MempoolAPI view m b
    -- ^ API for communication with mempool
  }


startNode
  :: ( MonadMask m, MonadFork m, MonadLogger m
     , Serialise (b Identity)
     , Serialise (b Proxy)
     , Serialise (Tx b)
     , StateView view m b
     )
  => NetCfg
  -> NetworkAPI
  -> [NetAddr]
  -> BlockDB   m b
  -> Consensus view m b
  -> ContT r m (PoW view m b)
startNode cfg netAPI seeds db consensus
  = fst <$> startNodeTest cfg netAPI seeds db consensus

-- | Same as startNode but expose internal interfacees for testing
startNodeTest
  :: ( MonadMask m, MonadFork m, MonadLogger m
     , Serialise (b Identity)
     , Serialise (b Proxy)
     , Serialise (Tx b)
     , StateView view m b
     )
  => NetCfg
  -> NetworkAPI
  -> [NetAddr]
  -> BlockDB   m b
  -> Consensus view m b
  -> ContT r m ( PoW view m b
               , Sink (BoxRX m b)
               )
startNodeTest cfg netAPI seeds db consensus = do
  lift $ logger InfoS "Starting PoW node" ()
  (sinkBOX,    srcBOX)     <- queuePair
  (sinkAnn,    mkSrcAnn)   <- broadcastPair
  (sinkChain,  mkSrcChain) <- broadcastPair
  (sinkBIDs,   srcBIDs)    <- queuePair
  blockReg                 <- newBlockRegistry srcBIDs
  bIdx                     <- liftIO $ newTVarIO consensus
  -- Start mempool
  (mempoolAPI,MempoolCh{..}) <- startMempool db (consensus ^. bestHead . _2)
  -- Start PEX
  runPEX cfg netAPI mempoolAPI mempoolAnnounces seeds blockReg sinkBOX mkSrcAnn (readTVar bIdx) db
  -- Consensus thread
  let consensusCh = ConsensusCh
        { bcastAnnounce    = sinkAnn
        , bcastChainUpdate = sinkChain
                          <> (contramap (\(bh,bh',s) -> MempHeadChange bh bh' s) mempoolConsensusCh)
        , sinkConsensusSt  = Sink $ writeTVar bIdx
        , sinkReqBlocks    = sinkBIDs
        , srcRX            = srcBOX
        }
  cforkLinked $ threadConsensus db consensus consensusCh
  return
    ( PoW { currentConsensus     = readTVar bIdx
          , sendNewBlock         = \(!b) -> do
              res <- liftIO newEmptyMVar
              sinkIO sinkBOX $ BoxRX $ \cnt -> liftIO . putMVar res =<< cnt (RxMined b)
              liftIO (takeMVar res) >>= \case
                Peer'Punish e     -> return (Left e)
                Peer'EnterCatchup -> return (Right ())
                Peer'Noop         -> return (Right ())
          , chainUpdate          = fmap (\(_,bh,s) -> (bh,s)) <$> mkSrcChain
          , ..
          }
    , sinkBOX
    )
