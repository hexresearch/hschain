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
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Foldable
import Data.Maybe
import Lens.Micro
import Katip (sl)

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
     , Serialise (b Identity)
     , Serialise (b Proxy)
     , Serialise (BlockID b)
     , BlockData b
     )
  => P2PConnection
  -> PeerChans s m b
  -> m ()
runPeer conn chans@PeerChans{..} = logOnException $ do
  logger InfoS "Starting peer" ()
  (sinkGossip, srcGossip) <- queuePair
  st <- liftIO $ do requestInFlight <- newTVarIO Nothing
                    peersBestHead   <- newTVarIO Nothing
                    inCatchup       <- newTVarIO False
                    return PeerState{..}
  -- Send announce with current state at start
  do s <- atomicallyIO peerConsensuSt
     sinkIO sinkGossip $ GossipAnn $ AnnBestHead $ s ^. bestHead . _1 . to asHeader
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
  { requestInFlight :: TVar (Maybe (SentRequest b))
    -- ^ Request that we sent to peer. We should only send one request
    --   at a time so this TMVar works as lock.
  , peersBestHead   :: TVar (Maybe (Header b))
    -- ^ Best head of peer (delivered by 'MsgAnn')
  , inCatchup       :: TVar Bool
    -- ^ Are we trying to catch up to peer?
  }


-- Thread that issues
peerRequestHeaders
  :: (MonadIO m, MonadLogger m, MonadCatch m, BlockData b)
  => PeerState b
  -> PeerChans s m b
  -> Sink (GossipMsg b)
  -> m x
peerRequestHeaders PeerState{..} PeerChans{..} sinkGossip =
  descendNamespace "req_H" $ logOnException $ forever $ atomicallyIO $ do
    -- Block unless we're in catchup and there's no requests in flight
    check             =<< readTVar inCatchup
    check . isNothing =<< readTVar requestInFlight
    -- If we don't know peer's best head exit catchup. Otherwise try
    -- to send request
    readTVar peersBestHead >>= \case
      Nothing -> exitCatchup
      Just h  -> do
        bidx <- peerConsensuSt
        case blockID h `lookupIdx` (bidx^.blockIndex) of
          Just _  -> exitCatchup
          Nothing -> do
            writeTVar requestInFlight . Just . SentHeaders =<< acquireCatchup peerCatchup
            st <- peerConsensuSt
            sink sinkGossip $ GossipReq $ ReqHeaders $ st^.bestHead._3
  where
    exitCatchup = writeTVar inCatchup False


peerRequestBlock
  :: (MonadIO m, MonadLogger m, MonadCatch m, BlockData b)
  => PeerState b
  -> PeerChans s m b
  -> Sink (GossipMsg b)
  -> m x
peerRequestBlock PeerState{..} PeerChans{..} sinkGossip =
  descendNamespace "req_B" $ logOnException $ forever $ do
    bid <- atomicallyIO $ do
      check . isNothing =<< readTVar requestInFlight
      bid <- reserveBID peerReqBlocks
      writeTVar requestInFlight $ Just $ SentBlock bid
      sink sinkGossip $ GossipReq $ ReqBlock bid
      return bid
    logger DebugS "Requesting BID" (sl "bid" bid)

peerRequestAddresses
  :: (MonadIO m, MonadLogger m, MonadMask m)
  => PeerState b -> PeerChans s m b -> Sink (GossipMsg b) -> m x
peerRequestAddresses PeerState{..} PeerChans{..} sinkGossip =
  descendNamespace "req_Addr" $ logOnException $ forever $ do
    AskPeers <- atomicallyIO $ await peerBCastAskPeer
    atomicallyIO $ do
      check . isNothing =<< readTVar requestInFlight
      writeTVar requestInFlight $ Just SentPeers
      sink sinkGossip $ GossipReq ReqPeers

-- Thread for sending messages over network
peerSend
  :: ( MonadIO m, MonadLogger m, MonadCatch m
     , Serialise (b Identity)
     , Serialise (b Proxy)
     , Serialise (BlockID b)
     )
  => P2PConnection
  -> Src (GossipMsg b)
  -> m x
peerSend conn src
  = descendNamespace "send" $ logOnException $ forever
  $ send conn . serialise =<< awaitIO src


-- Thread for receiving messages. It processes 'MsgRequest' in place
-- and routes other messages to respective handlers
peerRecv
  :: ( MonadMask m, MonadIO m, MonadLogger m
     , Serialise (b Identity)
     , Serialise (b Proxy)
     , Serialise (BlockID b)
     , BlockData b
     )
  => P2PConnection
  -> PeerState b
  -> PeerChans s m b
  -> Sink (GossipMsg b)         -- Send message to peer over network
  -> m x
peerRecv conn st@PeerState{..} PeerChans{..} sinkGossip =
  descendNamespace "recv" $ logOnException $ forever $ do
    bs <- recv conn
    case deserialise bs of
      -- Announces are just forwarded
      GossipAnn  m -> do
        toConsensus (return ()) $! RxAnn m
        case m of
          AnnBestHead h -> do logger DebugS "Got announce" (sl "bid" (blockID h))
                              atomicallyIO $ writeTVar peersBestHead (Just h)
      -- With responces we ensure that we got what we asked. Otherwise
      -- we throw exception and let PEX to deal with banning
      GossipResp m -> do
        logger DebugS "Responce" $ case m of
          RespHeaders hs -> (sl "headers" (map blockID hs))
          RespBlock   b  -> (sl "block"   (blockID b))
          RespPeers   as -> (sl "addrs"   as)
          RespNack       -> (sl "nack" ())
        liftIO (readTVarIO requestInFlight) >>= \case
          Nothing  -> throwM UnrequestedResponce
          Just req -> case (req, m) of
            (_              , RespNack     ) -> return ()
            (SentPeers      , RespPeers{}  ) -> return ()
            (SentHeaders rel, RespHeaders{}) -> atomicallyIO $ releaseCatchupLock rel
            (SentBlock   bid, RespBlock b  )
              | blockID b == bid          -> return ()
            _                             -> throwM InvalidResponce
        -- Process message and release request lock. Note that
        -- blocks/headers are processed in other thread and released
        -- in that thread.
        --
        -- NOTE: It could be simpler to block until consensus finish
        --       processing message but current approach allows thread
        --       to proceed immediately. At this point it's unclear
        --       whether performance benefit worth it.
        let release = atomicallyIO $ writeTVar requestInFlight Nothing
        case m of
          RespBlock   b -> toConsensus release $! RxBlock b
          RespHeaders h -> toConsensus release $! RxHeaders h
          RespPeers   a -> sinkIO peerSinkNewAddr a >> release
          RespNack      -> release
      -- We handle requests in place
      GossipReq  m -> case m of
        ReqPeers       ->
          sinkIO sinkGossip . GossipResp . RespPeers =<< atomicallyIO peerConnections
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
    -- Send message to consensus engine and release request lock when
    -- request is processed.
    toConsensus release m = do
      tid <- liftIO myThreadId
      sinkIO peerSinkConsensus $ BoxRX $ \cont -> do
        reactCommand tid st =<< cont m
        lift release

locateHeaders
  :: (BlockData b)
  => Consensus s m b
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
  Peer'Punish{}     -> liftIO $ throwTo tid ProtocolError
  Peer'EnterCatchup -> atomicallyIO $ writeTVar inCatchup True
  Peer'Noop         -> return ()
