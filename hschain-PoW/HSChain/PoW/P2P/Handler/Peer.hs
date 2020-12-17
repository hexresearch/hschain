{-# LANGUAGE NumDecimals  #-}
{-# LANGUAGE TypeFamilies #-}
-- |
module HSChain.PoW.P2P.Handler.Peer where

import Codec.Serialise
import Control.Concurrent (ThreadId,myThreadId,throwTo,threadDelay,forkIO)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Foldable
import Data.Maybe
import Katip (sl)

import HSChain.Control.Class
import HSChain.Control.Util
import HSChain.Control.Channels
import HSChain.Network.Types
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import HSChain.Logger
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
     , Serialise (Tx b)
     , BlockData b
     )
  => P2PConnection
  -> PeerChans m b
  -> m ()
runPeer conn chans@PeerChans{..} = logOnException $ do
  logger InfoS "Starting peer" ()
  (sinkGossip, srcGossip) <- queuePair
  st <- liftIO $ do requestInFlight <- newTVarIO Nothing
                    peersBestHead   <- newTVarIO Nothing
                    inCatchup       <- newTVarIO False
                    return PeerState{..}
  -- Send announce with current state at start
  do (_,bh,_) <- atomicallyIO peerConsensusState
     sinkIO sinkGossip $ GossipAnn $ AnnBestHead $ asHeader bh
  runConcurrently
    [ peerSend    conn (srcGossip <> peerBCastAnn)
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


-- Thread that requests header in case we're behind and need to catch up.
peerRequestHeaders
  :: (MonadIO m, MonadLogger m, MonadCatch m, BlockData b)
  => PeerState b
  -> PeerChans m b
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
        (bIdx,_,loc) <- peerConsensusState
        case blockID h `lookupIdx` bIdx of
          Just _  -> exitCatchup
          Nothing -> do
            writeTVar requestInFlight . Just . SentHeaders =<< acquireCatchup peerCatchup
            sink sinkGossip $ GossipReq $ ReqHeaders loc
  where
    exitCatchup = writeTVar inCatchup False

-- Thread that requests missing blocks
peerRequestBlock
  :: (MonadIO m, MonadLogger m, MonadCatch m, BlockData b)
  => PeerState b
  -> PeerChans m b
  -> Sink (GossipMsg b)
  -> m x
peerRequestBlock PeerState{..} PeerChans{..} sinkGossip =
  descendNamespace "req_B" $ logOnException $ forever $ do
    -- We simultaneously ask for block to fetch, reserve it so other
    -- threads won't request it and create timeout to release it
    -- automatically.
    --
    -- Mask is needed in order to avoid race when we acquired BID and
    -- killed before we started timeout thread.
    rBlk <- liftIO $ mask_ $ do
      rBlk <- atomically $ do
        check . isNothing =<< readTVar requestInFlight
        rBlk <- reserveBID peerReqBlocks
        writeTVar requestInFlight $ Just $ SentBlock rBlk
        sink sinkGossip $ GossipReq $ ReqBlock $ reservedBID rBlk
        return rBlk
      -- Timeout is hardcoded to 3s
      _ <- forkIO $ do threadDelay 3e6
                       atomically $ releaseOnFail rBlk
      pure rBlk
    logger DebugS "Requesting BID" ("bid" `sl` reservedBID rBlk)

peerRequestAddresses
  :: (MonadIO m, MonadLogger m, MonadMask m)
  => PeerState b -> PeerChans m b -> Sink (GossipMsg b) -> m x
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
     , Serialise (Tx b)
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
     , Serialise (Tx b)
     , BlockData b
     )
  => P2PConnection
  -> PeerState b
  -> PeerChans m b
  -> Sink (GossipMsg b)         -- Send message to peer over network
  -> m x
peerRecv conn st@PeerState{..} PeerChans{..} sinkGossip =
  descendNamespace "recv" $ logOnException $ forever $ do
    bs <- recv conn
    case deserialise bs of
      -- Announces are just forwarded
      GossipAnn  m -> case m of
          AnnBestHead h -> do logger DebugS "Got announce" (sl "bid" (blockID h))
                              toConsensus (return ()) $! RxAnn m
                              atomicallyIO $ writeTVar peersBestHead (Just h)
      -- With responces we ensure that we got what we asked. Otherwise
      -- we throw exception and let PEX to deal with banning
      GossipResp m -> do
        logger DebugS "Responce" $ case m of
          RespHeaders hs -> sl "headers" (map blockID hs)
          RespBlock   b  -> sl "block"   (blockID b)
          RespPeers   as -> sl "addrs"   as
          RespNack       -> sl "nack" ()
        -- Process message and release request lock. Note that
        -- blocks/headers are processed in other thread and released
        -- in that thread.
        --
        -- NOTE: It could be simpler to block until consensus finish
        --       processing message but current approach allows thread
        --       to proceed immediately. At this point it's unclear
        --       whether performance benefit worth it.p
        liftIO (readTVarIO requestInFlight) >>= \case
          Nothing  -> throwM UnrequestedResponce
          Just req -> case (req, m) of
            (SentPeers      , RespPeers   a) -> sinkIO peerSinkNewAddr a >> releaseReq
            (SentHeaders rel, RespHeaders h) -> do
              atomicallyIO $ releaseCatchupLock rel
              toConsensus releaseReq $! RxHeaders h
            -- When we didn't get block that we wanted we should release lock
            (SentBlock   rBlk, RespNack     ) -> atomicallyIO $ releaseOnFail rBlk
            (SentBlock   rBlk, RespBlock b  )
              | blockID b /= reservedBID rBlk -> do atomicallyIO $ releaseOnFail rBlk
                                                    throwM InvalidResponce
              | Just cb <- returnBlock rBlk   -> do atomicallyIO $ cb b
              | otherwise                     -> do toConsensus releaseReq $! RxBlock b
            -- Catchall clause
            _                             -> throwM InvalidResponce
      -- We handle requests in place
      GossipReq  m -> case m of
        ReqPeers       ->
          sinkIO sinkGossip . GossipResp . RespPeers =<< atomicallyIO peerConnections
        ReqHeaders loc -> do
          (bIdx,bh,_) <- atomicallyIO peerConsensusState
          sinkIO sinkGossip $ GossipResp $ case locateHeaders bIdx bh loc of
            Nothing -> RespNack
            Just hs -> RespHeaders hs
        ReqBlock   bid -> do
          mblk <- retrieveBlock peerBlockDB bid
          sinkIO sinkGossip $ GossipResp $ case mblk of
            Nothing -> RespNack
            Just b  -> RespBlock b
      -- Transactions gossip
      GossipTX m -> case m of
        AnnNewTX tx   -> do logger DebugS "Got TX announce" ()
                            sinkIO peerSinkTX tx

  where
    releaseReq :: MonadIO m => m ()
    releaseReq = atomicallyIO $ writeTVar requestInFlight Nothing
    -- Send message to consensus engine and release request lock when
    -- request is processed.
    toConsensus release m = do
      tid <- liftIO myThreadId
      sinkIO peerSinkConsensus $ BoxRX $ \cont -> do
        reactCommand tid st =<< cont m
        lift release


locateHeaders
  :: (BlockData b)
  => BlockIndex b -- ^ Block index
  -> BH b         -- ^ Current best head
  -> Locator b    -- ^ Request locator 
  -> Maybe [Header b]
locateHeaders bIdx best (Locator bidList) = do
  -- Find first index that we know about
  bh <- asum $ (`lookupIdx` bIdx) <$> bidList
  return $ traverseBlockIndex
    (\b -> (asHeader b :))
    (\_ -> id)
    best bh
    []


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
