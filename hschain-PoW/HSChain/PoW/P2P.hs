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
import Control.Concurrent.STM
import Control.Monad.Cont
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Catch

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


startNode
  :: ( MonadMask m, MonadFork m, MonadLogger m
     , Serialise (b IdNode)
     , Serialise (b Hashed)
     , Serialise (BlockID b)
     , BlockData b
     )
  => NetCfg
  -> NetworkAPI
  -> [NetAddr]
  -> BlockDB   m b
  -> Consensus m b
  -> m ()
startNode cfg netAPI seeds db consensus = evalContT $ do
  lift $ logger InfoS "Starting PoW node" ()
  (sinkBOX,    srcBOX)    <- queuePair
  (sinkAnn,    mkSrcAnn)  <- broadcastPair
  (sinkBIDs,   srcBIDs)   <- queuePair
  blockReg                <- newBlockRegistry srcBIDs
  bIdx                    <- liftIO $ newTVarIO consensus
  runPEX cfg netAPI seeds blockReg sinkBOX mkSrcAnn (readTVar bIdx) db
  -- Consensus thread
  lift $ threadConsensus db consensus ConsensusCh
    { bcastAnnounce   = sinkAnn
    , sinkConsensusSt = Sink $ writeTVar bIdx
    , sinkReqBlocks   = sinkBIDs
    , srcRX           = srcBOX
    }
