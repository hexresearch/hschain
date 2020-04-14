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
module HSChain.PoW.P2P.Handler.Peer where

import Codec.Serialise
import Control.Concurrent (ThreadId,myThreadId,throwTo)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Foldable
import Lens.Micro

import HSChain.Control.Class
import HSChain.Control.Util
import HSChain.Control.Channels
import HSChain.Network.Types
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import HSChain.PoW.Logger
import HSChain.PoW.Consensus
import HSChain.PoW.Exceptions
import HSChain.PoW.P2P.Handler.BlockRequests
import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- Public API
----------------------------------------------------------------

-- | Start peer execution when we already performed handshake and
--   registered peer
runPeer
  :: ( MonadMask m, MonadLogger m, MonadFork m
     , Serialise (b IdNode)
     , Serialise (b Hashed)
     , Serialise (BlockID b)
     , BlockData b
     )
  => P2PConnection
  -> PeerChans m b
  -> m ()
runPeer conn chans@PeerChans{..} = do
  (sinkGossip, srcGossip) <- queuePair
  st <- liftIO $ do requestInFlight <- newEmptyTMVarIO
                    peersBestHead   <- newTVarIO Nothing
                    inCatchup       <- newTVarIO False
                    return PeerState{..}
  runConcurrently
    [ peerSend    conn (srcGossip <> fmap GossipAnn peerBCastAnn)
    , peerRecv    conn     st chans sinkGossip
    , peerRequestHeaders   st chans sinkGossip
    , peerRequestBlock     st chans sinkGossip
    , peerRequestAddresses st chans sinkGossip
    ]


----------------------------------------------------------------
-- Internals
----------------------------------------------------------------

-- | Internal state of a peer
data PeerState b = PeerState
  { requestInFlight :: TMVar (SentRequest b)
    -- ^ Request that we sent to peer. We should only send one request
    --   at a time so this TMVar works as lock.
  , peersBestHead   :: TVar (Maybe (Header b))
    -- ^ Best head of peer (delivered by 'MsgAnn')
  , inCatchup       :: TVar Bool
    -- ^ Are we trying to catch up to peer?
  }


-- Thread that issues
peerRequestHeaders
  :: (MonadIO m, BlockData b)
  => PeerState b
  -> PeerChans m b
  -> Sink (GossipMsg b)
  -> m x
peerRequestHeaders PeerState{..} PeerChans{..} sinkGossip = forever $ do
  atomicallyIO $ do
    -- Block unless we're in catchup and there's no requests in flight
    check =<< readTVar inCatchup
    check =<< isEmptyTMVar requestInFlight
    -- If we don't know peer's best head exit catchup. Otherwise try
    -- to send request
    readTVar peersBestHead >>= \case
      Nothing -> exitCatchup
      Just h  -> do
        bidx <- peerConsensuSt
        case blockID h `lookupIdx` (bidx^.blockIndex) of
          Just _  -> exitCatchup
          Nothing -> do
            putTMVar requestInFlight . SentHeaders =<< acquireCatchup peerCatchup
            st <- peerConsensuSt
            sink sinkGossip $ GossipReq $ ReqHeaders $ st^.bestHead._3
  where
    exitCatchup = writeTVar inCatchup False


peerRequestBlock
  :: (MonadIO m)
  => PeerState b
  -> PeerChans m b
  -> Sink (GossipMsg b)
  -> m x
peerRequestBlock PeerState{..} PeerChans{..} sinkGossip = forever $ do
  atomicallyIO $ do
    check =<< isEmptyTMVar requestInFlight
    bid <- reserveBID peerReqBlocks
    putTMVar requestInFlight $ SentBlock bid
    sink sinkGossip $ GossipReq $ ReqBlock bid

peerRequestAddresses
  :: MonadIO m
  => PeerState b -> PeerChans m b -> Sink (GossipMsg b) -> m x
peerRequestAddresses PeerState{..} PeerChans{..} sinkGossip =
  forever $ atomicallyIO $ do
    AskPeers <- await peerBCastAskPeer
    putTMVar requestInFlight SentPeers
    sink sinkGossip $ GossipReq ReqPeers

-- Thread for sending messages over network
peerSend
  :: ( MonadMask m, MonadIO m
     , Serialise (b IdNode)
     , Serialise (b Hashed)
     , Serialise (BlockID b)
     )
  => P2PConnection
  -> Src (GossipMsg b)
  -> m x
peerSend conn src = forever $ send conn . serialise =<< awaitIO src


-- Thread for receiving messages. It processes 'MsgRequest' in place
-- and routes other messages to respective handlers
peerRecv
  :: ( MonadMask m, MonadIO m
     , Serialise (b IdNode)
     , Serialise (b Hashed)
     , Serialise (BlockID b)
     , BlockData b
     )
  => P2PConnection
  -> PeerState b
  -> PeerChans m b
  -> Sink (GossipMsg b)         -- Send message to peer over network
  -> m x
peerRecv conn st@PeerState{..} PeerChans{..} sinkGossip = forever $ do
  bs <- recv conn
  case deserialise bs of
    -- Announces are just forwarded
    GossipAnn  m -> do
      toConsensus $! RxAnn m
      case m of
        AnnBestHead h -> atomicallyIO $ writeTVar peersBestHead (Just h)
    -- With responces we ensure that we got what we asked. Otherwise
    -- we throw exception and let PEX to deal with banning
    GossipResp m -> do
      atomicallyIO (tryTakeTMVar requestInFlight) >>= \case
        Nothing  -> throwM UnrequestedResponce
        Just req -> case (req, m) of
          (_              , RespNack     ) -> return ()
          (SentPeers      , RespPeers{}  ) -> return ()
          (SentHeaders rel, RespHeaders{}) -> atomicallyIO $ releaseCatchupLock rel
          (SentBlock   bid, RespBlock b  )
            | blockID b == bid          -> return ()
          _                             -> throwM InvalidResponce
      -- Forward message
      case m of
        RespBlock   b -> toConsensus $! RxBlock b
        RespHeaders h -> toConsensus $! RxHeaders h
        RespPeers   a -> sinkIO sinkNewAddr a
        RespNack      -> return ()
    -- We handle requests in place
    GossipReq  m -> case m of
      ReqPeers       ->
        sinkIO sinkGossip . GossipResp . RespPeers =<< atomicallyIO pexGoodPeers
      ReqHeaders loc -> do
        c <- atomicallyIO peerConsensuSt
        sinkIO sinkGossip $ GossipResp $ case locateHeaders c loc of
          Nothing -> RespNack
          Just hs -> RespHeaders hs
      ReqBlock   bid -> do
        mblk <- retrieveBlock peerBlockDB bid
        sinkIO sinkGossip $ GossipResp $ case mblk of
          Nothing -> RespNack
          Just b  -> RespBlock b
  where
    toConsensus m = do tid <- liftIO myThreadId
                       sinkIO sinkConsensus $ BoxRX $ \cont -> reactCommand tid st =<< cont m

locateHeaders
  :: (BlockData b)
  => Consensus m b
  -> Locator b
  -> Maybe ([Header b])
locateHeaders consensus (Locator bidList) = do
  -- Find first index that we know about
  bh <- asum $ (`lookupIdx` bIdx) <$> bidList
  return $ traverseBlockIndex
    (\b -> ((asHeader b) :))
    (\_ -> id)
    best bh
    []
  where
    bIdx = consensus ^. blockIndex
    best = consensus ^. bestHead . _1

reactCommand
  :: (MonadIO m)
  => ThreadId
  -> PeerState b
  -> CmdPeer b
  -> m ()
reactCommand tid PeerState{..} = \case
  Peer'Punish       -> liftIO $ throwTo tid ProtocolError
  Peer'EnterCatchup -> atomicallyIO $ writeTVar inCatchup True
  Peer'Noop         -> return ()