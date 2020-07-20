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
import HSChain.Logger
import HSChain.PoW.P2P.Types
import HSChain.PoW.P2P.Handler.PEX
import HSChain.PoW.P2P.Handler.Consensus
import HSChain.PoW.P2P.Handler.BlockRequests
import HSChain.Types.Merkle.Types


data PoW s m b = PoW
  { currentConsensus     :: STM (Consensus s m b)
  , currentConsensusTVar :: TVar (Consensus s m b)
  , sendNewBlock         :: Block b -> m (Either SomeException ())
  , chainUpdate          :: STM (Src (BH b, StateView s m b))
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
  -> Consensus s m b
  -> ContT r m (PoW s m b)
startNode cfg netAPI seeds db consensus = do
  lift $ logger InfoS "Starting PoW node" ()
  (sinkBOX,    srcBOX)     <- queuePair
  (sinkAnn,    mkSrcAnn)   <- broadcastPair
  (sinkChain,  mkSrcChain) <- broadcastPair
  (sinkBIDs,   srcBIDs)    <- queuePair
  blockReg                 <- newBlockRegistry srcBIDs
  bIdx                     <- liftIO $ newTVarIO consensus
  runPEX cfg netAPI seeds blockReg sinkBOX mkSrcAnn (readTVar bIdx) db
  -- Consensus thread
  cforkLinked $ threadConsensus db consensus ConsensusCh
    { bcastAnnounce    = sinkAnn
    , bcastChainUpdate = sinkChain
    , sinkConsensusSt  = Sink $ writeTVar bIdx
    , sinkReqBlocks    = sinkBIDs
    , srcRX            = srcBOX
    }
  return PoW
    { currentConsensus     = readTVar bIdx
    , currentConsensusTVar = bIdx
    , sendNewBlock         = \(!b) -> runExceptT $ do
        res <- liftIO newEmptyMVar
        sinkIO sinkBOX $ BoxRX $ \cnt -> liftIO . putMVar res =<< cnt (RxMined b)
        void $ liftIO $ takeMVar res
    , chainUpdate          = mkSrcChain
    }
