{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
-- |
module HSChain.PoW.P2P where

import Codec.Serialise
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.Cont
import Control.Monad.Catch
import Control.Monad.Except

import HSChain.Control.Class
import HSChain.Control.Channels
import HSChain.Network.Types
import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.PoW.Logger
import HSChain.PoW.P2P.Types
import HSChain.PoW.P2P.Handler.PEX
import HSChain.PoW.P2P.Handler.Consensus
import HSChain.PoW.P2P.Handler.BlockRequests
import HSChain.Types.Merkle.Types


data PoW m b = PoW
  { currentConsensus :: STM (Consensus m b)
  , sendNewBlock     :: Block b -> m (Either SomeException ())
  }


startNode
  :: ( MonadMask m, MonadFork m, MonadLogger m
     , Serialise (b Identity)
     , Serialise (b Proxy)
     , Serialise (BlockID b)
     , BlockData b
     )
  => NetCfg
  -> NetworkAPI
  -> [NetAddr]
  -> BlockDB   m b
  -> Consensus m b
  -> ContT r m (PoW m b)
startNode cfg netAPI seeds db consensus = do
  lift $ logger InfoS "Starting PoW node" ()
  (sinkBOX,    srcBOX)    <- queuePair
  (sinkAnn,    mkSrcAnn)  <- broadcastPair
  (sinkBIDs,   srcBIDs)   <- queuePair
  blockReg                <- newBlockRegistry srcBIDs
  bIdx                    <- liftIO $ newTVarIO consensus
  runPEX cfg netAPI seeds blockReg sinkBOX mkSrcAnn (readTVar bIdx) db
  -- Consensus thread
  cfork $ threadConsensus db consensus ConsensusCh
    { bcastAnnounce   = sinkAnn
    , sinkConsensusSt = Sink $ writeTVar bIdx
    , sinkReqBlocks   = sinkBIDs
    , srcRX           = srcBOX
    }
  return PoW
    { currentConsensus = readTVar bIdx
    , sendNewBlock     = \(!b) -> runExceptT $ do
        res <- liftIO newEmptyMVar
        sinkIO sinkBOX $ BoxRX $ \cnt -> liftIO . putMVar res =<< cnt (RxMined b)
        void $ liftIO $ takeMVar res
    }


cfork :: (MonadMask m, MonadFork m) => m a -> ContT b m ()
cfork action = ContT $ \cnt -> forkLinked action (cnt ())
