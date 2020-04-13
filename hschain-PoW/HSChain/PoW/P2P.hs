{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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
import HSChain.PoW.P2P.Handler.PEX
import HSChain.PoW.P2P.Handler.Consensus
import HSChain.PoW.P2P.Handler.BlockRequests
import HSChain.Types.Merkle.Types


startNode
  :: ( MonadMask m, MonadFork m
     , Serialise (b IdNode)
     , Serialise (b Hashed)
     , Serialise (BlockID b)
     , BlockData b
     )
  => NetworkAPI
  -> BlockDB   m b
  -> Consensus m b
  -> m ()
startNode netAPI db consensus = evalContT $ do
  (sinkBOX,    srcBOX)    <- queuePair
  (sinkAnn,    mkSrcAnn)  <- broadcastPair
  (sinkBIDs,   srcBIDs)   <- queuePair
  blockReg                <- newBlockRegistry srcBIDs
  bIdx                    <- liftIO $ newTVarIO consensus
  runPEX netAPI blockReg sinkBOX mkSrcAnn (readTVar bIdx) db
  -- Consensus thread
  lift $ threadConsensus db consensus ConsensusCh
    { bcastAnnounce   = sinkAnn
    , sinkConsensusSt = Sink $ writeTVar bIdx
    , sinkReqBlocks   = sinkBIDs
    , srcRX           = srcBOX
    }
