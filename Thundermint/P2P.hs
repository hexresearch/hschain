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
import           Data.Maybe        (mapMaybe)
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
  peerSt   <- newPeerStateObj
    (hoistBlockStorageRO liftIO blockStorage)
    (hoistPropStorageRO  liftIO proposalStorage)
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
          FullStep peerH _ _
            | peerH == h
              -> liftIO $ retrieveLocalCommit blockStorage peerH
            | otherwise
              -> liftIO $ retrieveCommit blockStorage peerH
        --
        case mcmt of
         Just cmt -> do
           let cmtVotes  = Map.fromList [ (signedAddr v, unverifySignature v)
                                        | v <- commitPrecommits cmt ]
               -- FIXME: inefficient
           let toSet = Set.fromList
                     . map (address . validatorPubKey)
                     . mapMaybe (validatorByIndex (lagPeerValidators p))
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
                    . mapMaybe (validatorByIndex (peerValidators p))
                    . getValidatorIntSet

              unknownPV = Map.differenceWith remove knownPV (toSet <$> peerPrevotes   p)
              unknownPC = Map.differenceWith remove knownPC (toSet <$> peerPrecommits p)
          -- Send proposals
          case Map.lookupMin unknownPR of
            Nothing     -> return ()
            Just (_,pr) -> liftIO $ atomically $ writeTChan gossipCh $ GossipProposal $ unverifySignature pr
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
    getPeerState peerObj >>= \case
      --
      Lagging p
        | lagPeerHasProposal p
        , not (lagPeerHasBlock p)
          -> do let FullStep h _ _ = lagPeerStep p
                Just b <- liftIO $ retrieveBlock blockStorage h -- FIXME: Partiality
                liftIO $ atomically $ writeTChan gossipCh $ GossipBlock b
        | otherwise -> return ()
      --
      Current p -> do
        let FullStep h r _ = peerStep p
        mbid <- liftIO $ blockAtRound proposalStorage h r
        case () of
           -- Peer has proposal but not block
          _| Just (b,bid) <- mbid
           , r `Set.member` peerProposals p
           , not $ bid `Set.member` peerBlocks p
             -> do logger DebugS ("Gossip: " <> showLS bid) ()
                   liftIO $ atomically $ writeTChan gossipCh $ GossipBlock b
           --
           | otherwise -> return ()
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
                                  addProposal peerSt (propHeight (signedValue p))
                                                     (propRound  (signedValue p))
          GossipBlock     b -> do liftIO $ atomically $ peerChanRx $ RxBlock b
                                  addBlock peerSt b
          --
          GossipAnn ann -> case ann of
            AnnStep         s     -> advancePeer   peerSt s
            AnnHasProposal  h r   -> addProposal   peerSt h r
            AnnHasPreVote   h r i -> addPrevoteI   peerSt h r i
            AnnHasPreCommit h r i -> addPrecommitI peerSt h r i
            AnnHasBlock     h r   -> return ()
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
