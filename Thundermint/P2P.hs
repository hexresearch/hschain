{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Mock P2P
module Thundermint.P2P (
    startPeerDispatcher
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Concurrent   (ThreadId, myThreadId, threadDelay, killThread)
import Control.Concurrent.STM
import Codec.Serialise
import           Data.Monoid       ((<>))
-- import           Data.Foldable
import           Data.Function
import qualified Data.Map        as Map
import           Data.Map          (Map)
import qualified Data.Set        as Set
import           Data.Set          (Set)
import Katip        (showLS)
import GHC.Generics (Generic)


import Thundermint.Crypto
import Thundermint.Consensus.Types
import Thundermint.Blockchain.Types
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Control
import Thundermint.Logger


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Messages which peers exchange with each other
--
--   FIXME: We don't have good way to prevent DoS by spamming too much
--          data
data GossipMsg alg a
  = GossipPreVote   (Signed 'Unverified alg (Vote 'PreVote   alg a))
  | GossipPreCommit (Signed 'Unverified alg (Vote 'PreCommit alg a))
  | GossipProposal  (Signed 'Unverified alg (Proposal alg a))
  | GossipBlock     (Block alg a)

  -- Communication about status of peer

  | GossipStatus    Height Round
    -- ^ Gossip current status of consensus state machine
  | GossipHasProposals   Height (Set Round)
    -- ^ Gossip which proposals we have
    --
    --   FIXME: not space efficient. We could use bit array to
    --          represent proposals we have
  | GossipHasPrevotes    Height (Map Round (Set (Address alg)))
    -- ^ Gossip about prevotes that we have
  | GossipHasPrecommits  Height (Map Round (Set (Address alg)))
    -- ^ Gossip about precommits that we have
  | GossipHasPropBlocks  Height (Set (BlockID alg a))
    -- ^ Gossip which blocks do we have as proposals for current
    --   height

  -- Peer exchange

  | GossipHello
  | GossipRequestPeers
  | GossipPeers
  deriving (Show, Generic)
instance Serialise a => Serialise (GossipMsg alg a)


-- | Description of our knowledge of peer. It's need to keep track
--   what should we gossip to peer.
data PeerState alg a = PeerState
  { peerHeight     :: Height
    -- ^ Height peer at
  , peerPrevotes   :: Map Round (Set (Address alg))
    -- ^ Set of prevotes for current height that peer has
  , peerPrecommits :: Map Round (Set (Address alg))
    -- ^ Set of precommits for current height that peer has
  , peerProposals  :: Set Round
    -- ^ Set of proposals peer has
  , peerBlocks     :: Set (BlockID alg a)
    -- ^ Set of blocks known to peer
  }


-- | Connection handed to process controlling communication with peer
data PeerChans addr alg a = PeerChans
  { peerChanTx :: TChan (MessageTx alg a)
    -- ^ Broadcast channel for outgoing messages
  , peerChanRx :: MessageRx 'Unverified alg a -> STM ()
    -- ^ STM action for sending message to main application
  , retrievePeerSet :: STM (Set addr)
    -- ^ Obtain set of all peers
  , sendPeerSet     :: Set addr -> STM ()
    -- ^ Send set of peers to dispatcher
  , blockStorage    :: BlockStorage 'RO IO alg a
    -- ^ Read only access to storage of blocks
  }


----------------------------------------------------------------
-- Dispatcher
----------------------------------------------------------------

-- | Main process for networking. It manages accepting connections
--   from remote nodes, initiating connections to them, tracking state
--   of nodes and gossip.
startPeerDispatcher
  :: ( MonadMask m, MonadFork m, MonadLogger m
     , Serialise a, Ord addr, Show addr, Show a)
  => NetworkAPI sock addr       -- ^ API for networking
  -> [addr]                     -- ^ Set of initial addresses to connect
  -> AppChans alg a             -- ^ Channels for communication with main application
  -> BlockStorage 'RO IO alg a  -- ^ Read only access to block storage
  -> m x
startPeerDispatcher net addrs AppChans{..} storage = logOnException $ do
  logger InfoS "Starting peer dispatcher" ()
  peers        <- newPeerRegistry
  peerExchange <- liftIO newTChanIO
  let peerCh = PeerChans { peerChanTx      = appChanTx
                         , peerChanRx      = writeTChan appChanRx
                         , retrievePeerSet = registiryAddressSet peers
                         , sendPeerSet     = writeTChan peerExchange
                         , blockStorage    = storage
                         }
  -- Accepting connection is managed by separate linked thread and
  -- this thread manages initiating connections
  id $ flip finally (uninterruptibleMask_ $ reapPeers peers)
     $ forkLinked (acceptLoop net peerCh peers)
     -- FIXME: we should manage requests for peers and connecting to
     --        new peers here
     $ do liftIO $ threadDelay 100e3
          forM_ addrs $ \a -> connectPeerTo net a peerCh peers
          forever $ liftIO $ threadDelay 100000

-- Thread which accepts connections from remote nodes
acceptLoop
  :: ( MonadFork m, MonadMask m, MonadLogger m
     , Serialise a, Ord addr, Show addr, Show a)
  => NetworkAPI sock addr
  -> PeerChans addr alg a
  -> PeerRegistry addr
  -> m ()
acceptLoop net@NetworkAPI{..} peerCh registry = logOnException $ do
  logger InfoS "Starting accept loop" ()
  bracket (liftIO $ listenOn "50000") (liftIO . fst) $ \(_,accept) -> forever $
    -- We accept connection, create new thread and put it into
    -- registry. If we already have connection from that peer we close
    -- connection immediately
    mask $ \restore -> do
      (sock, addr) <- liftIO accept
      void $ flip forkFinally (const $ liftIO $ close sock)
           $ withPeer registry addr
           $ restore
           $ do logger InfoS ("Accepted connection from " <> showLS addr) ()
                startPeer peerCh (applySocket net sock)


-- Initiate connection to remote host and register peer
connectPeerTo
  :: ( MonadFork m, MonadMask m, MonadLogger m
     , Ord addr, Serialise a, Show addr, Show a
     )
  => NetworkAPI sock addr
  -> addr
  -> PeerChans addr alg a
  -> PeerRegistry addr
  -> m ()
connectPeerTo net@NetworkAPI{..} addr peerCh registry = do
  logger InfoS ("Connecting to " <> showLS addr) ()
  void $ fork
       $ bracket (liftIO $ connect addr) (liftIO . close)
       $ \sock -> withPeer registry addr
                $ startPeer peerCh (applySocket net sock)


-- Set of currently running peers.
data PeerRegistry a = PeerRegistry
                      (TVar (Map ThreadId a))
                      (TVar (Set a))
                      (TVar Bool)

-- Create new empty and active registry
newPeerRegistry :: MonadIO m => m (PeerRegistry a)
newPeerRegistry = PeerRegistry
               <$> liftIO (newTVarIO Map.empty)
               <*> liftIO (newTVarIO Set.empty)
               <*> liftIO (newTVarIO True)

-- Register peer using current thread ID. If we already have
-- registered peer with given address do nothing
withPeer :: (MonadMask m, MonadIO m, Ord addr)
         => PeerRegistry addr -> addr -> m () -> m ()
-- NOTE: we need to track activity of registry to avoid possibility of
--       successful registration after call to reapPeers
withPeer (PeerRegistry tidMap addrSet vActive) addr action = do
  tid <- liftIO myThreadId
  -- NOTE: we need uninterruptibleMask since we STM operation are
  --       blocking and they must not be interrupted
  uninterruptibleMask $ \restore -> do
    ok <- liftIO $ atomically $ registerPeer tid
    when ok $ restore action `finally` liftIO (atomically (unregisterPeer tid))
  where
    -- Add peer to registry and return whether it was success
    registerPeer tid = readTVar vActive >>= \case
      False -> return False
      True  -> do
        addrs <- readTVar addrSet
        case addr `Set.member` addrs of
          True  -> return False
          False -> do modifyTVar' tidMap  $ Map.insert tid addr
                      modifyTVar' addrSet $ Set.insert addr
                      return True
    -- Remove peer from registry
    unregisterPeer tid = readTVar vActive >>= \case
      False -> return ()
      True  -> do tids <- readTVar tidMap
                  case tid `Map.lookup` tids of
                    Nothing -> return ()
                    Just a  -> do modifyTVar' tidMap  $ Map.delete tid
                                  modifyTVar' addrSet $ Set.delete a

-- Kill all registered threads
reapPeers :: MonadIO m => PeerRegistry a -> m ()
reapPeers (PeerRegistry tidMap _ vActive) = liftIO $ do
  tids <- atomically $ do
    writeTVar vActive False
    readTVar tidMap
  mapM_ killThread $ Map.keys tids

registiryAddressSet :: PeerRegistry a -> STM (Set a)
registiryAddressSet (PeerRegistry _ addrSet _)
  = readTVar addrSet

----------------------------------------------------------------
-- Peer
----------------------------------------------------------------

startPeer
  :: (Serialise a, MonadFork m, MonadMask m, MonadLogger m, Show a)
  => PeerChans addr alg a  -- ^ Communication with main application
                           --   and peer dispatcher
  -> SendRecv              -- ^ Functions for interaction with network
  -> m ()
startPeer peerCh@PeerChans{..} net@SendRecv{..} = logOnException $ do
  logger InfoS "Starting peer" ()
  gossipCh <- liftIO $ newTChanIO
  peerVar  <- liftIO $ newTVarIO PeerState
    { peerHeight     = Height 0
    , peerPrevotes   = Map.empty
    , peerPrecommits = Map.empty
    , peerProposals  = Set.empty
    , peerBlocks     = Set.empty
    }
  -- Start gossip routines.
  id $ forkLinked (peerGossipBlocks peerCh gossipCh peerVar)
     $ forkLinked (peerGossipVotes  peerCh gossipCh peerVar)
     -- Start send thread.
     $ forkLinked (peerSendGossip gossipCh peerChanTx net)
     -- Receive data from socket.
     --
     -- FIXME: Implement framing for messages. At the moment we rely
     --        on implementation of MockNet where message won't be
     --        split or merged.
     $ fix $ \loop -> do
         message <- liftIO (recv 4096)
         case message of
           Nothing -> logger InfoS "Peer stopping since socket is closed" ()
           Just bs -> case deserialiseOrFail bs of
             -- FIXME: do something meaningful with decoding error.
             Left  e   -> logger ErrorS ("Deserialization error: " <> showLS e) ()
             Right msg -> do
              logger DebugS (showLS msg) ()
              case msg of
               -- Forward to application and record that peer has
               -- given vote/proposal/block
               GossipPreVote   v -> do liftIO $ atomically $ peerChanRx $ RxPreVote   v
                                       loop
               GossipPreCommit v -> do liftIO $ atomically $ peerChanRx $ RxPreCommit v
                                       loop
               GossipProposal  p -> do liftIO $ atomically $ peerChanRx $ RxProposal  p
                                       loop
               GossipBlock     b -> do liftIO $ atomically $ peerChanRx $ RxBlock     b
                                       loop
               -- Update peer state
               GossipStatus         h _          -> do
                 liftIO $ atomically $ modifyTVar' peerVar $ \p ->
                   if peerHeight p == h then p
                                        else PeerState { peerHeight     = h
                                                       , peerPrevotes   = Map.empty
                                                       , peerPrecommits = Map.empty
                                                       , peerProposals  = Set.empty
                                                       , peerBlocks     = Set.empty
                                                       }
                 loop
               GossipHasProposals   h props      -> do
                 liftIO $ atomically $ modifyTVar' peerVar $ \p ->
                   if peerHeight p == h then p
                                        else p { peerProposals = props }
                 loop
               GossipHasPrevotes    h prevotes   -> do
                 liftIO $ atomically $ modifyTVar' peerVar $ \p ->
                   if peerHeight p == h then p
                                        else p { peerPrevotes = prevotes }
                 loop
               GossipHasPrecommits  h precommtis -> do
                 liftIO $ atomically $ modifyTVar' peerVar $ \p ->
                   if peerHeight p == h then p
                                        else p { peerPrecommits = precommtis }
                 loop
               GossipHasPropBlocks  h bids       -> do
                 liftIO $ atomically $ modifyTVar' peerVar $ \p ->
                   if peerHeight p == h then p
                                        else p { peerBlocks = bids }
                 loop
               --
               GossipHello        -> loop
               GossipRequestPeers -> loop
               GossipPeers        -> loop



-- | Gossip blocks to peer
peerGossipBlocks
  :: (MonadIO m, MonadFork m, MonadCatch m, MonadLogger m)
  => PeerChans addr alg a
  -> TChan (GossipMsg alg a)
  -> TVar (PeerState alg a)
  -> m x
peerGossipBlocks PeerChans{..} _chan _peerVar = logOnException $ do
  logger InfoS "Starting routine for gossiping blocks" ()
  forever $ do
    -- st <- liftIO $ readTVarIO peerVar
    -- h  <- liftIO $ blockchainHeight blockStorage
    -- liftIO $ case h `compare` peerHeight st of
    --   -- We lag
    --   LT -> return ()
    --   -- We at the same height
    --   EQ -> do blocks <- retrievePropBlocks blockStorage h
    --            case Map.lookupMin $ Map.difference blocks $ Map.fromSet (const ()) $ peerBlocks st of
    --              Nothing    -> return ()
    --              Just (_,b) -> atomically $ writeTChan chan $ GossipBlock b
    --   -- Peer is lagging
    --   GT -> do Just bid <- retrieveBlockID blockStorage (peerHeight st)
    --            unless (bid `Set.member` peerBlocks st) $ do
    --              Just b <- retrieveBlock blockStorage (peerHeight st)
    --              atomically $ writeTChan chan $ GossipBlock b
    liftIO $ threadDelay 500e3

-- | Gossip votes with given peer
peerGossipVotes
  :: (MonadIO m, MonadFork m, MonadMask m, MonadLogger m)
  => PeerChans addr alg a
  -> TChan (GossipMsg alg a)
  -> TVar (PeerState alg a)
  -> m x
peerGossipVotes PeerChans{..} _chan _peerVar = logOnException $ do
  logger InfoS "Starting routine for gossiping votes" ()
  forever $ do
    -- st <- liftIO $ readTVarIO peerVar
    -- h  <- liftIO $ blockchainHeight blockStorage
    -- case h `compare` peerHeight st of
    --   -- We're lagging
    --   LT -> return ()
    --   -- We at the same height. Send prevote & precommit
    --   EQ -> return ()
    --   -- Peer is lagging. Send precommit
    --   GT -> return ()
    liftIO $ threadDelay 100e3


peerSendGossip
  :: (Serialise a, MonadIO m, MonadFork m, MonadMask m, MonadLogger m, Show a)
  => TChan (GossipMsg alg a)
  -> TChan (MessageTx alg a)
  -> SendRecv
  -> m x
peerSendGossip gossipCh chanTx SendRecv{..} = logOnException $ do
  ch <- liftIO $ atomically $ dupTChan chanTx
  logger InfoS "Starting routing for sending data" ()
  forever $ do
    logger DebugS "GSP: lock" ()
    msg <- liftIO $ atomically $ fromApp ch <|> readTChan gossipCh
    logger DebugS ("Sending[GSP] " <> showLS msg) ()
    liftIO $ send $ serialise msg
    where
      fromApp ch = readTChan ch >>= return . \case
        TxPreVote   v -> GossipPreVote   $ unverifySignature v
        TxPreCommit v -> GossipPreCommit $ unverifySignature v
        TxProposal  p -> GossipProposal  $ unverifySignature p
        TxBlock     b -> GossipBlock       b
