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
import           Data.Foldable
import           Data.Function
import qualified Data.Map        as Map
import           Data.Map          (Map)
import qualified Data.Set        as Set
import           Data.Set          (Set)
import Katip         (showLS)
import System.Random (randomRIO)
import GHC.Generics  (Generic)


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
data GossipMsg tx alg a
  = GossipPreVote   (Signed 'Unverified alg (Vote 'PreVote   alg a))
  | GossipPreCommit (Signed 'Unverified alg (Vote 'PreCommit alg a))
  | GossipProposal  (Signed 'Unverified alg (Proposal alg a))
  | GossipBlock     (Block alg a)
  | GossipAnn       (Announcement alg)
  | GossipTx        tx
  deriving (Show, Generic)
instance (Serialise tx, Serialise a) => Serialise (GossipMsg tx alg a)


-- | Connection handed to process controlling communication with peer
data PeerChans m addr alg a = PeerChans
  { peerChanTx      :: TChan (Announcement alg)
    -- ^ Broadcast channel for outgoing messages
  , peerChanRx      :: MessageRx 'Unverified alg a -> STM ()
    -- ^ STM action for sending message to main application
  , blockStorage    :: BlockStorage 'RO m alg a
    -- ^ Read only access to storage of blocks
  , proposalStorage :: ProposalStorage 'RO m alg a
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
     , Serialise a, Serialise tx, Ord addr, Show addr, Show a, Crypto alg)
  => NetworkAPI addr       -- ^ API for networking
  -> [addr]                     -- ^ Set of initial addresses to connect
  -> AppChans alg a             -- ^ Channels for communication with main application
  -> BlockStorage 'RO m alg a  -- ^ Read only access to block storage
  -> ProposalStorage 'RO m alg a
  -> Mempool m tx
  -> m x
startPeerDispatcher net addrs AppChans{..} storage propSt mempool = logOnException $ do
  logger InfoS "Starting peer dispatcher" ()
  peers        <- newPeerRegistry
  _peerExchange <- liftIO newTChanIO
  let peerCh = PeerChans { peerChanTx      = appChanTx
                         , peerChanRx      = writeTChan appChanRx
                         , blockStorage    = storage
                         , proposalStorage = propSt
                         , consensusState  = readTVar appTMState
                         }
  -- Accepting connection is managed by separate linked thread and
  -- this thread manages initiating connections
  id $ flip finally (uninterruptibleMask_ $ reapPeers peers)
     $ forkLinked (acceptLoop net peerCh mempool peers)
     -- FIXME: we should manage requests for peers and connecting to
     --        new peers here
     $ do liftIO $ threadDelay 100e3
          forM_ addrs $ \a -> connectPeerTo net a peerCh mempool peers
          forever $ liftIO $ threadDelay 100000

-- Thread which accepts connections from remote nodes
acceptLoop
  :: ( MonadFork m, MonadMask m, MonadLogger m
     , Serialise a, Serialise tx, Ord addr, Show addr, Show a, Crypto alg)
  => NetworkAPI addr
  -> PeerChans m addr alg a
  -> Mempool m tx
  -> PeerRegistry addr
  -> m ()
acceptLoop NetworkAPI{..} peerCh mempool registry = logOnException $ do
  logger InfoS "Starting accept loop" ()
  bracket (liftIO listenOn) (liftIO . fst) $ \(_,accept) -> forever $
    -- We accept connection, create new thread and put it into
    -- registry. If we already have connection from that peer we close
    -- connection immediately
    mask $ \restore -> do
      (conn, addr) <- liftIO accept
      void $ flip forkFinally (const $ liftIO $ close conn)
           $ withPeer registry addr
           $ restore
           $ do logger InfoS ("Accepted connection from " <> showLS addr) ()
                startPeer peerCh conn mempool


-- Initiate connection to remote host and register peer
connectPeerTo
  :: ( MonadFork m, MonadMask m, MonadLogger m
     , Ord addr, Serialise a, Serialise tx, Show addr, Show a, Crypto alg
     )
  => NetworkAPI addr
  -> addr
  -> PeerChans m addr alg a
  -> Mempool m tx
  -> PeerRegistry addr
  -> m ()
connectPeerTo NetworkAPI{..} addr peerCh mempool registry = do
  logger InfoS ("Connecting to " <> showLS addr) ()
  void $ fork
       $ bracket (liftIO $ connect addr) (liftIO . close)
       $ \conn -> withPeer registry addr
                $ startPeer peerCh conn mempool


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
  :: ( MonadFork m, MonadMask m, MonadLogger m
     , Show a, Serialise a, Serialise tx, Crypto alg)
  => PeerChans m addr alg a  -- ^ Communication with main application
                             --   and peer dispatcher
  -> Connection              -- ^ Functions for interaction with network
  -> Mempool m tx
  -> m ()
startPeer peerCh@PeerChans{..} net mempool = logOnException $ do
  logger InfoS "Starting peer" ()
  peerSt   <- newPeerStateObj blockStorage proposalStorage
  gossipCh <- liftIO newTChanIO
  cursor   <- getMempoolCursor mempool
  runConcurrently
    [ peerGossipVotes   peerSt peerCh gossipCh
    , peerGossipBlocks  peerSt peerCh gossipCh
    , peerGossipMempool peerSt gossipCh cursor
    , peerSend          peerSt peerCh gossipCh net
    , peerReceive       peerSt peerCh net cursor
    ]
  logger InfoS "Stopping peer" ()


-- | Gossip votes with given peer
peerGossipVotes
  :: (MonadIO m, MonadFork m, MonadMask m, MonadLogger m, Crypto alg)
  => PeerStateObj m alg a         -- ^ Current state of peer
  -> PeerChans m addr alg a       -- ^ Read-only access to
  -> TChan (GossipMsg tx alg a)
  -> m x
peerGossipVotes peerObj PeerChans{..} gossipCh = logOnException $ do
  logger InfoS "Starting routine for gossiping votes" ()
  forever $ do
    bchH <- blockchainHeight blockStorage
    peer <- getPeerState peerObj
    case peer of
      --
      Lagging p -> do
        mcmt <- case lagPeerStep p of
          FullStep peerH _ _
            | peerH == bchH -> retrieveLocalCommit blockStorage peerH
            | otherwise     -> retrieveCommit      blockStorage peerH
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
        Nothing                       -> return ()
        Just (h',_) | h' /= next bchH -> return ()
        Just (_,tm)                   -> do
          let FullStep _ r _ = peerStep p
              doGosip        = liftIO . atomically . writeTChan gossipCh
          -- FIXME: poor performance. Avoid going through map!
          let toSet = Set.fromList
                    . map (address . validatorPubKey)
                    . mapMaybe (validatorByIndex (peerValidators p))
                    . getValidatorIntSet

          -- Send proposals
          case () of
            _| not $ r `Set.member` peerProposals p
             , Just pr <- r `Map.lookup` smProposals tm
               -> doGosip $ GossipProposal $ unverifySignature pr
             | otherwise -> return ()
          -- Send prevotes
          case () of
            _| Just localPV <- Map.lookup r $ toPlainMap $ smPrevotesSet tm
             , unknown      <- Map.difference localPV peerPV
             , not (Map.null unknown)
               -> do let n = Map.size unknown
                     i <- liftIO $ randomRIO (0,n-1)
                     doGosip $ GossipPreVote $ unverifySignature $ toList unknown !! i
             | otherwise -> return ()
             where
               peerPV = maybe Map.empty (Map.fromSet (const ()) . toSet)
                      $ Map.lookup r $ peerPrevotes p
          -- Send precommits
          case () of
            _| Just localPC <- Map.lookup r $ toPlainMap $ smPrecommitsSet tm
             , unknown      <- Map.difference localPC peerPC
             , not (Map.null unknown)
               -> do let n = Map.size unknown
                     i <- liftIO $ randomRIO (0,n-1)
                     doGosip $ GossipPreCommit $ unverifySignature $ toList unknown !! i
             | otherwise -> return ()
             where
               peerPC = maybe Map.empty (Map.fromSet (const ()) . toSet)
                      $ Map.lookup r $ peerPrecommits p
      Ahead   _ -> return ()
      Unknown   -> return ()
    liftIO $ threadDelay 25e3

peerGossipMempool
  :: ( MonadIO m, MonadFork m, MonadMask m, MonadLogger m
     )
  => PeerStateObj m alg a
  -> TChan (GossipMsg tx alg a)
  -> MempoolCursor m tx
  -> m x
peerGossipMempool peerObj gossipCh MempoolCursor{..} = logOnException $ do
  logger InfoS "Starting routine for gossiping transactions" ()
  forever $ do
    getPeerState peerObj >>= \case
      Current _ -> advanceCursor >>= \case
        Just tx -> liftIO $ atomically $ writeTChan gossipCh $ GossipTx tx
        Nothing -> return ()
      _         -> return ()
    liftIO $ threadDelay 25e3
      
      

-- | Gossip blocks with given peer
peerGossipBlocks
  :: (MonadIO m, MonadFork m, MonadMask m, MonadLogger m)
  => PeerStateObj m alg a       -- ^ Current state of peer
  -> PeerChans m addr alg a     -- ^ Read-only access to
  -> TChan (GossipMsg tx alg a) -- ^ Network API
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
                Just b <- retrieveBlock blockStorage h -- FIXME: Partiality
                liftIO $ atomically $ writeTChan gossipCh $ GossipBlock b
        | otherwise -> return ()
      --
      Current p -> do
        let FullStep h r _ = peerStep p
        mbid <- blockAtRound proposalStorage h r
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
  :: ( MonadIO m, MonadFork m, MonadMask m, MonadLogger m
     , Crypto alg, Serialise a, Serialise tx)
  => PeerStateObj m alg a
  -> PeerChans m addr alg a
  -> Connection
  -> MempoolCursor m tx
  -> m ()
peerReceive peerSt PeerChans{..} Connection{..} MempoolCursor{..} = logOnException $ do
  logger InfoS "Starting routing for receiving messages" ()
  fix $ \loop -> liftIO recv >>= \case
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
          GossipTx tx       -> pushTransaction tx
          --
          GossipAnn ann -> case ann of
            AnnStep         s     -> advancePeer   peerSt s
            AnnHasProposal  h r   -> addProposal   peerSt h r
            AnnHasPreVote   h r i -> addPrevoteI   peerSt h r i
            AnnHasPreCommit h r i -> addPrecommitI peerSt h r i
            AnnHasBlock     h r   -> addBlockHR    peerSt h r
        loop

-- | Routine for actually sending data to peers
peerSend
  :: ( MonadIO m, MonadFork m, MonadMask m, MonadLogger m
     , Crypto alg, Serialise a, Serialise tx)
  => PeerStateObj m alg a
  -> PeerChans m addr alg a
  -> TChan (GossipMsg tx alg a)
  -> Connection
  -> m x
peerSend peerSt PeerChans{..} gossipCh Connection{..} = logOnException $ do
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
