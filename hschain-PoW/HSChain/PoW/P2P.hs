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
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Catch
import Data.Time       (UTCTime)
import Data.Map.Strict (Map)
import Data.Functor.Contravariant
import qualified Data.Map.Strict as Map
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import HSChain.Control.Class
import HSChain.Control.Util
import HSChain.Control.Channels
import HSChain.Control.Shepherd
import HSChain.Network.Types
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.PoW.Exceptions
import HSChain.PoW.P2P.Handler.PEX
import HSChain.PoW.P2P.Handler.Consensus
import HSChain.PoW.P2P.Handler.BlockRequests
import HSChain.Types.Merkle.Types
import HSChain.Crypto

-- startNode ::
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
  (sinkBOX,srcBOX)   <- queuePair
  (sinkReqBID, srcReqBID) <- queuePair
  (sinkAnn,mkSrcAnn) <- broadcastPair
  bIdx               <- liftIO $ newTVarIO $ consensus ^. blockIndex
  runPEX netAPI sinkBOX mkSrcAnn (readTVar bIdx) db
  -- Consensus thread
  lift $ threadConsensus db consensus ConsensusCh
    { bcastAnnounce = sinkAnn
    , sinkBlockIdx  = Sink $ writeTVar bIdx
    , sinkReqBlocks = sinkReqBID
    , srcRX         = srcBOX
    }
