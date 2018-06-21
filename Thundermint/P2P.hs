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
import           Data.Maybe        (fromMaybe,catMaybes)
-- import           Data.Foldable
import           Data.Function
import qualified Data.Map        as Map
import           Data.Map          (Map)
import qualified Data.Set        as Set
import           Data.Set          (Set)
import Katip        (showLS)
import GHC.Generics (Generic)


import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Consensus.Types
import Thundermint.Blockchain.Types
import Thundermint.P2P.Network
import Thundermint.P2P.PeerState
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
  | GossipAnn       (Announcement alg)
  deriving (Show, Generic)
instance Serialise a => Serialise (GossipMsg alg a)


-- | Connection handed to process controlling communication with peer
data PeerChans addr alg a = PeerChans
  { peerChanTx      :: TChan (Announcement alg)
    -- ^ Broadcast channel for outgoing messages
  , peerChanRx      :: MessageRx 'Unverified alg a -> STM ()
    -- ^ STM action for sending message to main application
  , blockStorage    :: BlockStorage 'RO IO alg a
    -- ^ Read only access to storage of blocks
  , proposalStorage :: ProposalStorage 'RO IO alg a
    -- ^ Read only access to storage of proposals
  , consensusState  :: STM (Maybe (Height, TMState alg a))
    -- ^ Read only access to current state of consensus state machine
  }


----------------------------------------------------------------
-- Dispatcher
----------------------------------------------------------------

-- | Main process for networking. It manages accepting connections
--   from remote nodes, initiating connections to them, tracking state
--   of nodes and gossip.
startPeerDispatcher
  :: ( MonadMask m, MonadFork m, MonadLogger m
     , Serialise a, Ord addr, Show addr, Show a, Crypto alg)
  => NetworkAPI sock addr       -- ^ API for networking
  -> [addr]                     -- ^ Set of initial addresses to connect
  -> AppChans alg a             -- ^ Channels for communication with main application
  -> BlockStorage 'RO IO alg a  -- ^ Read only access to block storage
  -> ProposalStorage 'RO IO alg a
  -> m x
startPeerDispatcher net addrs AppChans{..} storage propSt = logOnException $ do
  logger InfoS "Starting peer dispatcher" ()
  peers        <- newPeerRegistry
  peerExchange <- liftIO newTChanIO
  let peerCh = PeerChans { peerChanTx      = appChanTx
                         , peerChanRx      = writeTChan appChanRx
                         , blockStorage    = storage
                         , proposalStorage = propSt
                         , consensusState  = readTVar appTMState
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
     , Serialise a, Ord addr, Show addr, Show a, Crypto alg)
  => NetworkAPI sock addr
  -> PeerChans addr alg a
  -> PeerRegistry addr
  -> m ()
acceptLoop net@NetworkAPI{..} peerCh registry = logOnException $ do
  logger InfoS "Starting accept loop" ()
  bracket (liftIO listenOn) (liftIO . fst) $ \(_,accept) -> forever $
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
     , Ord addr, Serialise a, Show addr, Show a, Crypto alg
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

{-
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

peerStateAtH :: Height -> PeerState alg a
peerStateAtH h = PeerState { peerHeight     = h
                           , peerPrevotes   = Map.empty
                           , peerPrecommits = Map.empty
                           , peerProposals  = Set.empty
                           , peerBlocks     = Set.empty
                           }

addProposal :: Signed ty alg (Proposal alg a)
            -> PeerState alg a -> PeerState alg a
addProposal svote ps
  | peerHeight ps < h = addProposal svote $ peerStateAtH h
  | otherwise         = ps { peerProposals = Set.insert r (peerProposals ps) }
  where
    h = propHeight (signedValue svote)
    r = propRound  (signedValue svote)

addPrevote :: Signed ty alg (Vote 'PreVote alg a)
           -> PeerState alg a -> PeerState alg a
addPrevote svote = addPrevote'
  (voteHeight (signedValue svote))
  (voteRound  (signedValue svote))
  (signedAddr svote)

addPrevote' :: Height -> Round -> Address alg -> PeerState alg a -> PeerState alg a
addPrevote' h r addr ps
  | peerHeight ps < h = addPrevote' h r addr $ peerStateAtH h
  | otherwise         = ps { peerPrevotes = Map.alter add r (peerPrevotes ps) }
  where
    add Nothing  = Just $ Set.singleton addr
    add (Just s) = Just $ Set.insert addr s

addPrecommit :: Signed ty alg (Vote 'PreCommit alg a)
             -> PeerState alg a -> PeerState alg a
addPrecommit svote ps
  | peerHeight ps < h = addPrecommit svote $ peerStateAtH h
  | otherwise         = ps { peerPrecommits = Map.alter add r (peerPrecommits ps) }
  where
    h = voteHeight (signedValue svote)
    r = voteRound  (signedValue svote)
    add Nothing  = Just $ Set.singleton $ signedAddr svote
    add (Just s) = Just $ Set.insert (signedAddr svote) s

addPrecommit' :: Height -> Round -> Address alg -> PeerState alg a -> PeerState alg a
addPrecommit' h r addr ps
  | peerHeight ps < h = addPrevote' h r addr $ peerStateAtH h
  | otherwise         = ps { peerPrecommits = Map.alter add r (peerPrecommits ps) }
  where
    add Nothing  = Just $ Set.singleton addr
    add (Just s) = Just $ Set.insert addr s

addBlock :: (Crypto alg, Serialise a) => Block alg a -> PeerState alg a -> PeerState alg a
addBlock b ps = ps { peerBlocks = Set.insert (blockHash b) (peerBlocks ps) }
-}

----------------------------------------------------------------
-- Peer's status
----------------------------------------------------------------

-- | Start interactions with peer. At this point connection is already
--   established and peer is registered.
startPeer
  :: (Serialise a, MonadFork m, MonadMask m, MonadLogger m, Show a, Crypto alg)
  => PeerChans addr alg a  -- ^ Communication with main application
                           --   and peer dispatcher
  -> SendRecv              -- ^ Functions for interaction with network
  -> m ()
startPeer peerCh@PeerChans{..} net@SendRecv{..} = logOnException $ do
  logger InfoS "Starting peer" ()
  peerSt   <- newPeerStateObj $ hoistBlockStorageRO liftIO blockStorage
  gossipCh <- liftIO newTChanIO
  runConcurrently
    [ peerGossipVotes   peerSt peerCh gossipCh
    , peerGossipBlocks  peerSt peerCh gossipCh
    , peerSend          peerSt peerCh gossipCh net
    , peerReceive       peerSt peerCh net
    ]
  logger InfoS "Stopping peer" ()


-- | Gossip votes with given peer
peerGossipVotes
  :: (MonadIO m, MonadFork m, MonadMask m, MonadLogger m, Crypto alg)
  => PeerStateObj m alg a         -- ^ Current state of peer
  -> PeerChans addr alg a       -- ^ Read-only access to
  -> TChan (GossipMsg alg a)
  -> m x
peerGossipVotes peerObj PeerChans{..} gossipCh = logOnException $ do
  logger InfoS "Starting routine for gossiping votes" ()
  forever $ do
    h    <- liftIO $ blockchainHeight blockStorage
    peer <- getPeerState peerObj
    case peer of
      --
      Lagging p -> do
        mcmt <- case lagPeerStep p of
          FullStep (Height 0) _ _
            -> return Nothing -- FIXME: Possible???
          FullStep peerH _ _
            | peerH == h
              -> liftIO $ retrieveLocalCommit blockStorage peerH
            | otherwise
              -> liftIO $ retrieveCommit blockStorage peerH
        --
        case mcmt of
         Just cmt -> do
           let r         = voteRound $ signedValue $ head $ commitPrecommits cmt
               cmtVotes  = Map.fromList [ (signedAddr v, unverifySignature v)
                                        | v <- commitPrecommits cmt ]
               -- FIXME: inefficient
           let toSet = Set.fromList
                     . map (address . validatorPubKey)
                     . catMaybes
                     . map (validatorByIndex (lagPeerValidators p))
                     . getValidatorIntSet
           let peerVotes = Map.fromSet (const ())
                         $ toSet (lagPeerPrecommits p)
           case Map.lookupMin $ Map.difference cmtVotes peerVotes of
             Just (_,v) -> liftIO $ atomically $ writeTChan gossipCh $ GossipPreCommit v
             Nothing    -> return ()
         Nothing -> return ()

      --
      Current p -> liftIO (atomically consensusState) >>= \case
        Nothing               -> return ()
        Just (h',_) | h' /= next h -> return ()
        Just (_,tm)           -> do
          let knownPR = smProposals tm
              knownPV = toPlainMap $ smPrevotesSet   tm
              knownPC = toPlainMap $ smPrecommitsSet tm
              remove votes addrs
                | Map.null d = Nothing
                | otherwise  = Just d
                where d = Map.difference votes (Map.fromSet (const ()) addrs)
              --
              unknownPR = Map.difference knownPR (Map.fromSet (const ()) (peerProposals p))
              -- FIXME: inefficient
              toSet = Set.fromList
                    . map (address . validatorPubKey)
                    . catMaybes
                    . map (validatorByIndex (peerValidators p))
                    . getValidatorIntSet

              unknownPV = Map.differenceWith remove knownPV (toSet <$> peerPrevotes   p)
              unknownPC = Map.differenceWith remove knownPC (toSet <$> peerPrecommits p)
          -- Send proposals
          case Map.lookupMin unknownPR of
            Nothing    -> return ()
            Just (_,p) -> liftIO $ atomically $ writeTChan gossipCh $ GossipProposal $ unverifySignature p
          -- Send prevotes
          case Map.lookupMin . snd =<< Map.lookupMin unknownPV of
            Nothing    -> return ()
            Just (_,v) -> liftIO $ atomically $ writeTChan gossipCh $ GossipPreVote $ unverifySignature v
          -- Send precommits
          case Map.lookupMin . snd =<< Map.lookupMin unknownPC of
            Nothing    -> return ()
            Just (_,v) -> liftIO $ atomically $ writeTChan gossipCh $ GossipPreCommit $ unverifySignature v
      Ahead   _ -> return ()
      Unknown   -> return ()
    liftIO $ threadDelay 25e3

-- | Gossip blocks with given peer
peerGossipBlocks
  :: (MonadIO m, MonadFork m, MonadMask m, MonadLogger m)
  => PeerStateObj m alg a       -- ^ Current state of peer
  -> PeerChans addr alg a       -- ^ Read-only access to
  -> TChan (GossipMsg alg a)    -- ^ Network API
  -> m x
peerGossipBlocks peerObj PeerChans{..} gossipCh = logOnException $ do
  logger InfoS "Starting routine for gossiping votes" ()
  forever $ do
    peer <- getPeerState peerObj
    case peer of
      --
      Lagging p -> do
        let FullStep h _ _ = lagPeerStep p
        mbid <- liftIO $ retrieveBlockID blockStorage h
        case mbid of
          Just bid | bid `Set.notMember` lagPeerBlocks p -> do
                       Just b <- liftIO $ retrieveBlock blockStorage h
                       logger DebugS ("Gossip: " <> showLS bid) ()
                       liftIO $ atomically $ writeTChan gossipCh $ GossipBlock b
          _ -> return ()
      --
      Current p -> do
        h      <- liftIO $ blockchainHeight blockStorage
        blocks <- liftIO $ retrievePropBlocks proposalStorage $ next h
        case Map.lookupMin $ Map.difference blocks $ Map.fromSet (const ()) $ peerBlocks p of
          Nothing    -> return ()
          Just (bid,b) -> do
            logger DebugS ("Gossip: " <> showLS bid) ()
            liftIO $ atomically $ writeTChan gossipCh $ GossipBlock b
      -- Nothing to do
      Ahead _ -> return ()
      Unknown -> return ()
    liftIO $ threadDelay 25e3

-- | Routine for receiving messages from peer
peerReceive
  :: (MonadIO m, MonadFork m, MonadMask m, MonadLogger m, Crypto alg, Serialise a)
  => PeerStateObj m alg a
  -> PeerChans addr alg a
  -> SendRecv
  -> m ()
peerReceive peerSt PeerChans{..} SendRecv{..} = logOnException $ do
  logger InfoS "Starting routing for receiving messages" ()
  fix $ \loop -> liftIO (recv 4096) >>= \case
    Nothing  -> logger InfoS "Peer stopping since socket is closed" ()
    Just bs  -> case deserialiseOrFail bs of
      Left  e   -> logger ErrorS ("Deserialization error: " <> showLS e) ()
      Right msg -> do
        case msg of
          -- Forward to application and record that peer has
          -- given vote/proposal/block
          GossipPreVote   v -> do liftIO $ atomically $ peerChanRx $ RxPreVote v
                                  addPrevote peerSt v
          GossipPreCommit v -> do liftIO $ atomically $ peerChanRx $ RxPreCommit v
                                  addPrecommit peerSt v
          GossipProposal  p -> do liftIO $ atomically $ peerChanRx $ RxProposal p
                                  addProposal peerSt p
          GossipBlock     b -> do liftIO $ atomically $ peerChanRx $ RxBlock b
                                  addBlock peerSt b
          --
          GossipAnn ann -> case ann of
            AnnStep         s     -> advancePeer   peerSt s
            AnnHasPreVote   h r i -> addPrevoteI   peerSt h r i
            AnnHasPreCommit h r i -> addPrecommitI peerSt h r i
        loop

-- | Routine for actually sending data to peers
peerSend
  :: (MonadIO m, MonadFork m, MonadMask m, MonadLogger m, Crypto alg, Serialise a)
  => PeerStateObj m alg a
  -> PeerChans addr alg a
  -> TChan (GossipMsg alg a)
  -> SendRecv
  -> m x
peerSend peerSt PeerChans{..} gossipCh SendRecv{..} = logOnException $ do
  logger InfoS "Starting routing for sending data" ()
  ch <- liftIO $ atomically $ dupTChan peerChanTx
  forever $ do
    msg <- liftIO $ atomically $  readTChan gossipCh
                              <|> fmap GossipAnn (readTChan ch)
    liftIO $ send $ serialise msg
    -- Update state of peer when we advance to next height
    case msg of
      GossipBlock b                        -> addBlock peerSt b
      GossipAnn (AnnStep (FullStep h _ _)) -> advanceOurHeight peerSt h
      _                                    -> return ()


{-


----------------------------------------------------------------

  gossipCh <- liftIO $ newTChanIO
  peerVar  <- liftIO $ newTVarIO $ peerStateAtH (Height 0)
  -- Start gossip routines.





  id $ forkLinked (peerGossipBlocks peerCh gossipCh peerVar)
     $ forkLinked (peerGossipVotes  peerCh gossipCh peerVar)
     -- Start send thread.
     $ forkLinked (peerSendGossip gossipCh peerChanTx peerVar net)
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
               GossipPreVote   v -> do liftIO $ atomically $ peerChanRx $ RxPreVote v
                                       liftIO $ atomically $ modifyTVar' peerVar $ addPrevote v
                                       loop
               GossipPreCommit v -> do liftIO $ atomically $ peerChanRx $ RxPreCommit v
                                       liftIO $ atomically $ modifyTVar' peerVar $ addPrecommit v
                                       loop
               GossipProposal  p -> do liftIO $ atomically $ peerChanRx $ RxProposal p
                                       liftIO $ atomically $ modifyTVar' peerVar $ addProposal p
                                       loop
               GossipBlock     b -> do liftIO $ atomically $ peerChanRx $ RxBlock b
                                       liftIO $ atomically $ modifyTVar peerVar $ addBlock b
                                       loop
               -- Update peer state
               GossipStatus         h _          -> do
                 liftIO $ atomically $ modifyTVar' peerVar $ \p ->
                   if peerHeight p == h then p else peerStateAtH h
                 loop
               GossipHasVote h r PreVote a -> do
                 liftIO $ atomically $ modifyTVar' peerVar $ addPrevote' h r a
                 loop
               GossipHasVote h r PreCommit a -> do
                 liftIO $ atomically $ modifyTVar' peerVar $ addPrecommit' h r a
                 loop

               -- VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
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
peerGossipBlocks PeerChans{..} chan peerVar = logOnException $ do
  logger InfoS "Starting routine for gossiping blocks" ()
  forever $ do
    st <- liftIO $ readTVarIO peerVar
    h  <- liftIO $ currentHeight proposalStorage
    case h `compare` peerHeight st of
      -- We lag
      LT -> return ()
      -- We at the same height
      EQ -> do blocks <- liftIO $ retrievePropBlocks proposalStorage h
               case Map.lookupMin $ Map.difference blocks $ Map.fromSet (const ()) $ peerBlocks st of
                 Nothing    -> return ()
                 Just (bid,b) -> do
                   logger DebugS ("Gossip: " <> showLS bid) ()
                   liftIO $ atomically $ writeTChan chan $ GossipBlock b
      -- Peer is lagging
      GT -> do mbid <- liftIO $ retrieveBlockID blockStorage (peerHeight st)
               case mbid of
                 Just bid | bid `Set.notMember` peerBlocks st -> do
                              Just b <- liftIO $ retrieveBlock blockStorage (peerHeight st)
                              logger DebugS ("Gossip: " <> showLS bid) ()
                              liftIO $ atomically $ writeTChan chan $ GossipBlock b
                 _ -> return ()
    liftIO $ threadDelay 25e3

-- | Gossip votes with given peer
peerGossipVotes
  :: (MonadIO m, MonadFork m, MonadMask m, MonadLogger m)
  => PeerChans addr alg a
  -> TChan (GossipMsg alg a)
  -> TVar (PeerState alg a)
  -> m x
peerGossipVotes PeerChans{..} chan peerVar = logOnException $ do
  logger InfoS "Starting routine for gossiping votes" ()
  forever $ do
    st <- liftIO $ readTVarIO peerVar
    h  <- liftIO $ blockchainHeight blockStorage
    case h `compare` peerHeight st of
      -- We're lagging
      LT -> return ()
      -- We at the same height. Send prevote & precommit
      EQ -> liftIO (atomically consensusState) >>= \case
        Nothing               -> return ()
        Just (h',_) | h' /= h -> return ()
        Just (_,tm)           -> do
          let knownPR = smProposals tm
              knownPV = toPlainMap $ smPrevotesSet   tm
              knownPC = toPlainMap $ smPrecommitsSet tm
              remove votes addrs
                | Map.null d = Nothing
                | otherwise  = Just d
                where d = Map.difference votes (Map.fromSet (const ()) addrs)
              --
              unknownPR = Map.difference knownPR (Map.fromSet (const ()) (peerProposals st))
              unknownPV = Map.differenceWith remove knownPV (peerPrevotes   st)
              unknownPC = Map.differenceWith remove knownPC (peerPrecommits st)
          -- Send proposals
          case Map.lookupMin unknownPR of
            Nothing    -> return ()
            Just (_,p) -> liftIO $ atomically $ writeTChan chan $ GossipProposal $ unverifySignature p
          -- Send prevotes
          case Map.lookupMin . snd =<< Map.lookupMin unknownPV of
            Nothing    -> return ()
            Just (_,v) -> liftIO $ atomically $ writeTChan chan $ GossipPreVote $ unverifySignature v
          -- Send precommits
          case Map.lookupMin . snd =<< Map.lookupMin unknownPC of
            Nothing    -> return ()
            Just (_,v) -> liftIO $ atomically $ writeTChan chan $ GossipPreCommit $ unverifySignature v
      -- Peer is lagging. Send precommits from commit for that round
      GT -> do mcmt <- liftIO $ retrieveCommit blockStorage (next $ peerHeight st)
               case (mcmt, peerHeight st) of
                 (Just cmt, _) -> do
                   let r         = voteRound $ signedValue $ head $ commitPrecommits cmt
                       cmtVotes  = Map.fromList [ (signedAddr v, unverifySignature v)
                                                | v <- commitPrecommits cmt ]
                       peerVotes = Map.fromSet (const ())
                                 $ fromMaybe Set.empty
                                 $ r `Map.lookup` peerPrecommits st
                   case Map.lookupMin $ Map.difference cmtVotes peerVotes of
                     Just (_,v) -> liftIO $ atomically $ writeTChan chan $ GossipPreCommit v
                     Nothing    -> return ()
                 (Nothing, Height 0) -> return ()
                 _                   -> error "Inconsistent database state"
    liftIO $ threadDelay 25e3


peerSendGossip
  :: (Serialise a, MonadIO m, MonadFork m, MonadMask m, MonadLogger m, Show a, Crypto alg)
  => TChan (GossipMsg alg a)
  -> TChan (MessageTx alg a)
  -> TVar  (PeerState alg a)
  -> SendRecv
  -> m x
peerSendGossip gossipCh chanTx peerVar SendRecv{..} = logOnException $ do
  ch <- liftIO $ atomically $ dupTChan chanTx
  logger InfoS "Starting routing for sending data" ()
  forever $ do
    msg <- liftIO $ atomically $ fromApp ch <|> readTChan gossipCh
    logger DebugS ("Sending[GSP] " <> showLS msg) ()
    case msg of
      GossipPreVote   v     -> liftIO $ atomically $ modifyTVar' peerVar $ addPrevote   v
      GossipPreCommit v     -> liftIO $ atomically $ modifyTVar' peerVar $ addPrecommit v
      GossipProposal  p     -> liftIO $ atomically $ modifyTVar' peerVar $ addProposal  p
      GossipBlock     b     -> liftIO $ atomically $ modifyTVar' peerVar $ addBlock     b
      GossipHasVote{}       -> return ()
      GossipStatus{}        -> return ()
      GossipHasProposals{}  -> return ()
      GossipHasPrevotes{}   -> return ()
      GossipHasPrecommits{} -> return ()
      GossipHasPropBlocks{} -> return ()
      GossipHello{}         -> return ()
      GossipRequestPeers{}  -> return ()
      GossipPeers{}         -> return ()
    liftIO $ send $ serialise msg
    where
      fromApp ch = readTChan ch >>= return . \case
        TxPreVote    v       -> GossipPreVote   $ unverifySignature v
        TxPreCommit  v       -> GossipPreCommit $ unverifySignature v
        TxProposal   p       -> GossipProposal  $ unverifySignature p
        TxAnnHasVote h r s a -> GossipHasVote h r s a
-}
