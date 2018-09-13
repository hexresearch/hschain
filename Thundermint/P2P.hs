{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- |
-- Mock P2P
module Thundermint.P2P (
    startPeerDispatcher
  , LogGossip(..)
  ) where

import Codec.Serialise
import Control.Applicative
import Control.Concurrent      ( ThreadId, myThreadId, threadDelay, killThread
                               , MVar, readMVar, newMVar)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry           (recoverAll)
import Data.Default.Class      (def)
import           Data.Monoid       ((<>))
import           Data.Maybe        (mapMaybe)
import           Data.Foldable
import           Data.Function
import qualified Data.Map        as Map
import           Data.Map          (Map)
import qualified Data.Set        as Set
import           Data.Set          (Set)
import qualified Data.Aeson      as JSON
import qualified Data.Aeson.TH   as JSON
import qualified Data.Text       as T
import Katip         (showLS,sl)
import qualified Katip
import System.Random (randomRIO)
import GHC.Generics  (Generic)


import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.P2P.Network
import Thundermint.P2P.PeerState
import Thundermint.Store


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Messages which peers exchange with each other
--
--   FIXME: We don't have good way to prevent DoS by spamming too much
--          data
data GossipMsg tx addr alg a
  = GossipPreVote   (Signed 'Unverified alg (Vote 'PreVote   alg a))
  | GossipPreCommit (Signed 'Unverified alg (Vote 'PreCommit alg a))
  | GossipProposal  (Signed 'Unverified alg (Proposal alg a))
  | GossipBlock     (Block alg a)
  | GossipAnn       (Announcement alg)
  | GossipTx        tx
  | GossipPex       (PexMessage addr)
  deriving (Show, Generic)

instance (Serialise tx, Serialise addr, Serialise a) => Serialise (GossipMsg tx addr alg a)


-- | Peer exchage gossip sub-message
--
data PexMessage addr
  = PexMsgAskForMorePeers
  -- ^ Peer need yet connections to peers
  | PexMsgMorePeers [addr]
  -- ^ Some addresses of other connected peers
  | PexPing
  -- ^ Message to estimate connection speed between peers
  | PexPong
  -- ^ Answer for Ping
  deriving (Show, Generic)

instance (Serialise addr) => Serialise (PexMessage addr)


data LogGossip = LogGossip
  { gossip'TxPV  :: !Int
  , gossip'RxPV  :: !Int
  , gossip'TxPC  :: !Int
  , gossip'RxPC  :: !Int
  , gossip'TxB   :: !Int
  , gossip'RxB   :: !Int
  , gossip'TxP   :: !Int
  , gossip'RxP   :: !Int
  , gossip'TxTx  :: !Int
  , gossip'RxTx  :: !Int
  , gossip'TxPex :: !Int
  , gossip'RxPex :: !Int
  }
  deriving (Show)
JSON.deriveJSON JSON.defaultOptions
  { JSON.fieldLabelModifier = drop 7 } ''LogGossip
instance Katip.ToObject LogGossip
instance Katip.LogItem  LogGossip where
  payloadKeys _        _ = Katip.AllKeys


-- | Connection handed to process controlling communication with peer
data PeerChans m addr alg a = PeerChans
  { peerChanTx      :: TChan (Announcement alg)
    -- ^ Broadcast channel for outgoing messages
  , peerChanPex     :: TChan (PexMessage addr)
    -- ^ Broadcast channel for outgoing PEX messages
  , peerChanRx      :: MessageRx 'Unverified alg a -> STM ()
    -- ^ STM action for sending message to main application
  , blockStorage    :: BlockStorage 'RO m alg a
    -- ^ Read only access to storage of blocks
  , proposalStorage :: ProposalStorage 'RO m alg a
    -- ^ Read only access to storage of proposals
  , consensusState  :: STM (Maybe (Height, TMState alg a))
    -- ^ Read only access to current state of consensus state machine
  , p2pConfig       :: Configuration

  , cntGossipPrevote   :: Counter
  , cntGossipPrecommit :: Counter
  , cntGossipBlocks    :: Counter
  , cntGossipProposals :: Counter
  , cntGossipTx        :: Counter
  , cntGossipPex       :: Counter
  }

-- | Counter for counting send/receive event
data Counter = Counter (MVar Int) (MVar Int)

newCounter :: MonadIO m => m Counter
newCounter = Counter <$> liftIO (newMVar 0) <*>liftIO (newMVar 0)

tickSend :: (MonadMask m, MonadIO m) => Counter -> m ()
tickSend (Counter s _) = modifyMVarM_ s (return . succ)

tickRecv :: (MonadMask m, MonadIO m) => Counter -> m ()
tickRecv (Counter _ r) = modifyMVarM_ r (return . succ)

readSend :: MonadIO m => Counter -> m Int
readSend (Counter s _) = liftIO $ readMVar s

readRecv :: MonadIO m => Counter -> m Int
readRecv (Counter _ r) = liftIO $ readMVar r


----------------------------------------------------------------
-- Dispatcher
----------------------------------------------------------------

-- | Main process for networking. It manages accepting connections
--   from remote nodes, initiating connections to them, tracking state
--   of nodes and gossip.
startPeerDispatcher
  :: ( MonadMask m, MonadFork m, MonadLogger m, MonadTrace m
     , Serialise a, Serialise tx, Ord addr, Show addr, Serialise addr, Show a, Crypto alg)
  => Configuration
  -> NetworkAPI addr          -- ^ API for networking
  -> addr                     -- ^ Current peer address
  -> [addr]                   -- ^ Set of initial addresses to connect
  -> AppChans m alg a         -- ^ Channels for communication with main application
  -> BlockStorage 'RO m alg a -- ^ Read only access to block storage
  -> Mempool m alg tx
  -> m ()
startPeerDispatcher p2pConfig net peerAddr addrs AppChans{..} storage mempool = logOnException $ do
  logger InfoS "Starting peer dispatcher" ()
  trace TeNodeStarted
  peerRegistry       <- newPeerRegistry
  peerChanPex       <- liftIO newBroadcastTChanIO
  cntGossipPrevote   <- newCounter
  cntGossipPrecommit <- newCounter
  cntGossipProposals <- newCounter
  cntGossipBlocks    <- newCounter
  cntGossipTx        <- newCounter
  cntGossipPex       <- newCounter
  let peerCh = PeerChans { peerChanTx      = appChanTx
                         , peerChanPex     = peerChanPex
                         , peerChanRx      = writeTChan appChanRx
                         , blockStorage    = storage
                         , proposalStorage = makeReadOnlyPS appPropStorage
                         , consensusState  = readTVar appTMState
                         , ..
                         }
  -- Accepting connection is managed by separate linked thread and
  -- this thread manages initiating connections
  flip finally (uninterruptibleMask_ $ reapPeers peerRegistry) $ runConcurrently
    -- Makes 5 attempts to start acceptLoop wirh 5 msec interval
    [ recoverAll def $ const $ logOnException $
        acceptLoop peerAddr net peerCh mempool peerRegistry
     -- FIXME: we should manage requests for peers and connecting to
     --        new peers here
    , do liftIO $ threadDelay 1e5
         forM_ addrs $ \a ->
             -- Makes 5 attempts to start acceptLoop wirh 5 msec interval
             recoverAll def $ const $ logOnException $
               connectPeerTo peerAddr net a peerCh mempool peerRegistry
         forever $ liftIO $ threadDelay 100000
    -- -- Peer connection monitor
    -- , peerPexMonitor peerAddr net peerCh mempool peerRegistry
    -- Output gossip statistics to
    , forever $ do
        logGossip peerCh
        liftIO $ threadDelay 1e6
    ]


-- Thread which accepts connections from remote nodes
acceptLoop
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadTrace m
     , Serialise a, Serialise tx, Ord addr, Show addr, Serialise addr, Show a, Crypto alg)
  => addr
  -> NetworkAPI addr
  -> PeerChans m addr alg a
  -> Mempool m alg tx
  -> PeerRegistry addr
  -> m ()
acceptLoop peerAddr NetworkAPI{..} peerCh mempool peerRegistry = logOnException $ do
  logger InfoS "Starting accept loop" ()
  bracket listenOn fst $ \(_,accept) -> forever $
    -- We accept connection, create new thread and put it into
    -- registry. If we already have connection from that peer we close
    -- connection immediately
    mask $ \restore -> do
      (conn, addr) <- accept
      trace $ TeNodeOtherTryConnect (show addr)
      void $ flip forkFinally (const $ close conn)
           $ restore
           $ withPeer peerRegistry addr -- XXX what first? `withPeer` or `restore` ??
           $ do logger InfoS "Accepted connection" (sl "addr" (show addr))
                trace $ TeNodeOtherConnected (show addr)
                descendNamespace (T.pack (show addr))
                  $ startPeer peerAddr peerCh conn peerRegistry mempool


-- Initiate connection to remote host and register peer
connectPeerTo
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadTrace m
     , Ord addr, Serialise a, Serialise tx, Show addr, Serialise addr, Show a, Crypto alg
     )
  => addr
  -> NetworkAPI addr
  -> addr
  -> PeerChans m addr alg a
  -> Mempool m alg tx
  -> PeerRegistry addr
  -> m ()
connectPeerTo peerAddr NetworkAPI{..} addr peerCh mempool peerRegistry =
  -- Igrnore all exceptions to prevent apparing of error messages in stderr/stdout.
  void . flip forkFinally (const $ return ()) . logOnException $ do
    logger InfoS "Connecting to" (sl "addr" (show addr))
    trace (TeNodeConnectingTo (show addr))
    bracket (connect addr) close
      $ \conn -> withPeer peerRegistry addr
               $ startPeer peerAddr peerCh conn peerRegistry mempool


-- Set of currently running peers
data PeerRegistry addr = PeerRegistry
    { prTidMap        :: TVar (Map ThreadId addr)
      -- ^ Threads that process connection to address
    , prConnected     :: TVar (Set addr)
      -- ^ Connected addresses
    , prKnownAddreses :: TVar (Set addr)
      -- ^ New addresses to connect
    , prIsActive      :: TVar Bool
      -- ^ `False` when close all connections
    }


-- Create new empty and active registry
newPeerRegistry :: MonadIO m => m (PeerRegistry addr)
newPeerRegistry = PeerRegistry
               <$> liftIO (newTVarIO Map.empty)
               <*> liftIO (newTVarIO Set.empty)
               <*> liftIO (newTVarIO Set.empty)
               <*> liftIO (newTVarIO True)

-- Register peer using current thread ID. If we already have
-- registered peer with given address do nothing
withPeer :: (MonadMask m, MonadIO m, MonadTrace m, Ord addr, Show addr)
         => PeerRegistry addr -> addr -> m () -> m ()
-- NOTE: we need to track activity of registry to avoid possibility of
--       successful registration after call to reapPeers
withPeer (PeerRegistry{..}) addr action = do
  tid <- liftIO myThreadId
  -- NOTE: we need uninterruptibleMask since we STM operation are
  --       blocking and they must not be interrupted
  uninterruptibleMask $ \restore -> do
    (ok, addrs) <- liftIO $ atomically $ registerPeer tid
    when ok $
        restore (tracePRChange addrs >> action)
        `finally`
        (liftIO (atomically (unregisterPeer tid)) >>= tracePRChange)
  where
    tracePRChange addrs = trace $ TePeerRegistryChanged (Set.map show addrs)
    -- Add peer to registry and return whether it was success
    registerPeer tid = readTVar prIsActive >>= \case
      False -> return (False, Set.empty)
      True  -> do
        addrs <- readTVar prConnected
        case addr `Set.member` addrs of
          True  -> return (False, addrs)
          False -> do modifyTVar' prTidMap    $ Map.insert tid addr
                      modifyTVar' prConnected $ Set.insert addr
                      addrs' <- readTVar prConnected
                      return (True, addrs')
    -- Remove peer from registry
    unregisterPeer tid = readTVar prIsActive >>= \case
      False -> return Set.empty
      True  -> do tids <- readTVar prTidMap
                  case tid `Map.lookup` tids of
                    Nothing -> return Set.empty
                    Just a  -> do modifyTVar' prTidMap    $ Map.delete tid
                                  modifyTVar' prConnected $ Set.delete a
                                  readTVar prConnected

-- Kill all registered threads
reapPeers :: MonadIO m => PeerRegistry addr -> m ()
reapPeers PeerRegistry{..} = liftIO $ do
  tids <- atomically $ do
    writeTVar prIsActive False
    readTVar prTidMap
  mapM_ killThread $ Map.keys tids

----------------------------------------------------------------
-- Peer exchange
----------------------------------------------------------------


ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM predicate thenAct elseAct = do
    predicate >>= \case
        True  -> thenAct
        False -> elseAct


whenM :: (Monad m) => m Bool -> m () -> m ()
whenM predicate act = ifM predicate act (return ())


peerPexMonitor
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadTrace m
     , Serialise a, Serialise tx, Ord addr, Show addr, Serialise addr, Show a, Crypto alg)
  => addr -- ^ Current peer address for logging purpose
  -> NetworkAPI addr
  -> PeerChans m addr alg a
  -> Mempool m alg tx
  -> PeerRegistry addr
  -> m ()
peerPexMonitor peerAddr net peerCh mempool peerRegistry@PeerRegistry{..} = do
    -- v1: Послать всем другим пирам запрос на новые адреса.
    --     Затем подключиться к ним всем.
    -- v2: Подключиться только к N..M пирам.
    --     Остальные адреса хранить (на будущее).
    --     Следить за количеством подключений.
    --     Если оно падает меньше N, переподключаться к произвольным до тех пор, пока не станет M.
    -- v3: Периодически пинговать подключения (и реже -- остальные адреса).
    --     Отключаться от медленных подключений, подключаться к более быстрым.
    -- v4: Ввести разделение на seed / sentry / full node / (security) validators
    -- v5: ?
    --
    --
    -- Now v1 implementation: --
    liftIO $ threadDelay 1e6 -- wait until all peers connected
    fix $ \loop -> do
        whenM (liftIO $ atomically $ readTVar prIsActive) $ do
            (conns, knowns') <- liftIO $ atomically $ (,) <$> readTVar prConnected <*> readTVar prKnownAddreses
            let notConnectedKnowns = peerAddr `Set.delete` (knowns' Set.\\ conns)
            if Set.size conns < 10 then do -- TODO get count from config
                logger InfoS ("PEX " <> showLS peerAddr <> ": Too small (" <> showLS (Set.size conns) <> ") connections") ()
                when (Set.null notConnectedKnowns) $ do
                    logger InfoS ("PEX " <> showLS peerAddr <> ": Ask for peers") ()
                    liftIO $ atomically $ writeTChan (peerChanPex peerCh) PexMsgAskForMorePeers
                -- TODO: Potentially, during endless wait, other peer can connect to this.
                --       So we need to send to that peer PexMsgAskForMorePeers for more peers!
                --       It can be done by adding flag `isNeedPeers` in PeerRegistry and immediately
                --       send message when new connection occur.
                newAddrs <- liftIO $ atomically $ do
                    ifM (readTVar prIsActive)
                        (do
                            knowns <- readTVar prKnownAddreses
                            connected <- readTVar prConnected
                            let newKnowns = peerAddr `Set.delete` (knowns Set.\\ connected)
                            check $ not $ Set.null newKnowns
                            return $ Just newKnowns
                            )
                        (return Nothing)
                logger InfoS ("PEX " <> showLS peerAddr <> ": new peers: " <> showLS newAddrs) ()
                case newAddrs of
                    Just newAddrs' -> do
                        forM_ newAddrs' $ \addr -> connectPeerTo peerAddr net addr peerCh mempool peerRegistry
                        liftIO $ threadDelay 1e6 -- wait some time to initialize peers
                    Nothing -> return ()
            else do
                liftIO $ threadDelay 1e7 -- wait more time to potential disconnect
            loop


peerGossipPeerExchange
  :: ( MonadIO m, MonadFork m, MonadMask m, MonadLogger m
     , Show a, Serialise a, Serialise addr, Show addr, Ord addr, Crypto alg)
  => addr
  -> PeerChans m addr alg a
  -> PeerRegistry addr
  -> TChan (PexMessage addr)
  -> TChan (GossipMsg tx addr alg a)
  -> m ()
peerGossipPeerExchange peerAddr _peerCh PeerRegistry{..} pexCh gossipCh = forever $ do
    liftIO (atomically $ readTChan pexCh) >>= \case
        PexMsgAskForMorePeers -> sendPeers
        PexMsgMorePeers addrs -> connectToAddrs addrs
        PexPing               -> ping
        PexPong               -> pong
  where
    sendPeers = do
        logger InfoS ("PEX " <> showLS peerAddr <> ": some peer ask for other peers") ()
        liftIO $ atomically $ do
            readTVar prIsActive >>= \case
                False -> return ()
                True -> do
                    addrList <- Set.toList <$> readTVar prConnected
                    when (not $ null addrList) $
                        writeTChan gossipCh (GossipPex (PexMsgMorePeers addrList))
    connectToAddrs addrs = do
        logger InfoS ("PEX " <> showLS peerAddr <> ": some peers received: " <> showLS addrs) ()
        liftIO $ atomically $ modifyTVar' prKnownAddreses (Set.union (Set.fromList addrs))
    ping = liftIO $ atomically $ writeTChan gossipCh (GossipPex PexPong)
    pong = return ()


----------------------------------------------------------------
-- Peer
----------------------------------------------------------------

-- | Start interactions with peer. At this point connection is already
--   established and peer is registered.
startPeer
  :: ( MonadFork m, MonadMask m, MonadLogger m
     , Show a, Serialise a, Serialise addr, Show addr, Ord addr, Serialise tx, Crypto alg)
  => addr
  -> PeerChans m addr alg a  -- ^ Communication with main application
                             --   and peer dispatcher
  -> Connection              -- ^ Functions for interaction with network
  -> PeerRegistry addr
  -> Mempool m alg tx
  -> m ()
startPeer peerAddr peerCh@PeerChans{..} conn peerRegistry mempool = logOnException $ do
  logger InfoS "Starting peer" ()
  peerSt   <- newPeerStateObj blockStorage proposalStorage
  gossipCh <- liftIO newTChanIO
  pexCh    <- liftIO newTChanIO
  cursor   <- getMempoolCursor mempool
  runConcurrently
    [ descendNamespace "gspV" $ peerGossipVotes         peerSt peerCh gossipCh
    , descendNamespace "gspB" $ peerGossipBlocks        peerSt peerCh gossipCh
    , descendNamespace "gspM" $ peerGossipMempool       peerSt peerCh p2pConfig gossipCh cursor
    -- , peerGossipPeerExchange  peerAddr peerCh peerRegistry pexCh gossipCh
    , descendNamespace "send" $ peerSend                peerSt peerCh gossipCh conn
    , descendNamespace "recv" $ peerReceive             peerSt peerCh pexCh conn cursor
    ]
  logger InfoS "Stopping peer" ()


-- | Gossip votes with given peer
peerGossipVotes
  :: (MonadIO m, MonadFork m, MonadMask m, MonadLogger m, Crypto alg)
  => PeerStateObj m alg a         -- ^ Current state of peer
  -> PeerChans m addr alg a       -- ^ Read-only access to
  -> TChan (GossipMsg tx addr alg a)
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
               unknown   = Map.difference cmtVotes peerVotes
           case Map.size unknown of
             0 -> return ()
             n -> do i <- liftIO $ randomRIO (0,n-1)
                     liftIO $ atomically $ writeTChan gossipCh $ GossipPreCommit
                       $ unverifySignature $ toList unknown !! i
                     tickSend cntGossipPrecommit
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
               -> do doGosip $ GossipProposal $ unverifySignature pr
                     tickSend cntGossipProposals
             | otherwise -> return ()
          -- Send prevotes
          case () of
            _| Just localPV <- Map.lookup r $ toPlainMap $ smPrevotesSet tm
             , unknown      <- Map.difference localPV peerPV
             , not (Map.null unknown)
               -> do let n = Map.size unknown
                     i <- liftIO $ randomRIO (0,n-1)
                     doGosip $ GossipPreVote $ unverifySignature $ toList unknown !! i
                     tickSend cntGossipPrevote
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
                     tickSend cntGossipPrecommit
             | otherwise -> return ()
             where
               peerPC = maybe Map.empty (Map.fromSet (const ()) . toSet)
                      $ Map.lookup r $ peerPrecommits p
      Ahead   _ -> return ()
      Unknown   -> return ()
    liftIO $ threadDelay $ 1000 * gossipDelayVotes p2pConfig

peerGossipMempool
  :: ( MonadIO m, MonadFork m, MonadMask m, MonadLogger m
     )
  => PeerStateObj m alg a
  -> PeerChans m addr alg a
  -> Configuration
  -> TChan (GossipMsg tx addr alg a)
  -> MempoolCursor m alg tx
  -> m x
peerGossipMempool peerObj PeerChans{..} config gossipCh MempoolCursor{..} = logOnException $ do
  logger InfoS "Starting routine for gossiping transactions" ()
  forever $ do
    getPeerState peerObj >>= \case
      Current _ -> advanceCursor >>= \case
        Just tx -> do liftIO $ atomically $ writeTChan gossipCh $ GossipTx tx
                      tickSend cntGossipTx
        Nothing -> return ()
      _         -> return ()
    liftIO $ threadDelay $ 1000 * gossipDelayMempool config


-- | Gossip blocks with given peer
peerGossipBlocks
  :: (MonadIO m, MonadFork m, MonadMask m, MonadLogger m)
  => PeerStateObj m alg a       -- ^ Current state of peer
  -> PeerChans m addr alg a     -- ^ Read-only access to
  -> TChan (GossipMsg tx addr alg a) -- ^ Network API
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
                tickSend cntGossipBlocks
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
                   tickSend cntGossipBlocks
           --
           | otherwise -> return ()
      -- Nothing to do
      Ahead _ -> return ()
      Unknown -> return ()
    liftIO $ threadDelay $ 1000 * gossipDelayBlocks p2pConfig

-- | Routine for receiving messages from peer
peerReceive
  :: ( MonadIO m, MonadFork m, MonadMask m, MonadLogger m
     , Serialise addr, Crypto alg, Serialise a, Serialise tx)
  => PeerStateObj m alg a
  -> PeerChans m addr alg a
  -> TChan (PexMessage addr)
  -> Connection
  -> MempoolCursor m alg tx
  -> m ()
peerReceive peerSt PeerChans{..} peerExchangeCh Connection{..} MempoolCursor{..} = logOnException $ do
  logger InfoS "Starting routing for receiving messages" ()
  fix $ \loop -> recv >>= \case
    Nothing  -> logger InfoS "Peer stopping since socket is closed" ()
    Just bs  -> case deserialiseOrFail bs of
      Left  e   -> logger ErrorS ("Deserialization error: " <> showLS e) ()
      Right msg -> do
        case msg of
          -- Forward to application and record that peer has
          -- given vote/proposal/block
          GossipPreVote   v -> do liftIO $ atomically $ peerChanRx $ RxPreVote v
                                  tickRecv cntGossipPrevote
                                  addPrevote peerSt v
          GossipPreCommit v -> do liftIO $ atomically $ peerChanRx $ RxPreCommit v
                                  tickRecv cntGossipPrecommit
                                  addPrecommit peerSt v
          GossipProposal  p -> do liftIO $ atomically $ peerChanRx $ RxProposal p
                                  tickRecv cntGossipProposals
                                  addProposal peerSt (propHeight (signedValue p))
                                                     (propRound  (signedValue p))
          GossipBlock     b -> do liftIO $ atomically $ peerChanRx $ RxBlock b
                                  tickRecv cntGossipBlocks
                                  addBlock peerSt b
          GossipTx tx       -> do pushTransaction tx
                                  tickRecv cntGossipTx
          GossipPex pexmsg  -> do liftIO $ atomically $ writeTChan peerExchangeCh pexmsg
                                  tickRecv cntGossipPex
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
     , Crypto alg, Serialise addr, Serialise a, Serialise tx)
  => PeerStateObj m alg a
  -> PeerChans m addr alg a
  -> TChan (GossipMsg tx addr alg a)
  -> Connection
  -> m x
peerSend peerSt PeerChans{..} gossipCh Connection{..} = logOnException $ do
  logger InfoS "Starting routing for sending data" ()
  ownPeerChanTx  <- liftIO $ atomically $ dupTChan peerChanTx
  ownPeerChanPex <- liftIO $ atomically $ dupTChan peerChanPex
  forever $ do
    msg <- liftIO $ atomically $  readTChan gossipCh
                              <|> fmap GossipAnn (readTChan ownPeerChanTx)
                              <|> fmap GossipPex (readTChan ownPeerChanPex)
    send $ serialise msg
    -- Update state of peer when we advance to next height
    case msg of
      GossipBlock b                        -> addBlock peerSt b
      GossipAnn (AnnStep (FullStep h _ _)) -> advanceOurHeight peerSt h
      _                                    -> return ()


logGossip
  :: ( MonadIO m, MonadFork m, MonadMask m, MonadLogger m )
  => PeerChans m addr alg a
  -> m ()
logGossip PeerChans{..} = do
  gossip'TxPV  <- readSend cntGossipPrevote
  gossip'RxPV  <- readRecv cntGossipPrevote
  gossip'TxPC  <- readSend cntGossipPrecommit
  gossip'RxPC  <- readRecv cntGossipPrecommit
  gossip'TxB   <- readSend cntGossipBlocks
  gossip'RxB   <- readRecv cntGossipBlocks
  gossip'TxP   <- readSend cntGossipProposals
  gossip'RxP   <- readRecv cntGossipProposals
  gossip'TxTx  <- readSend cntGossipTx
  gossip'RxTx  <- readRecv cntGossipTx
  gossip'TxPex <- readSend cntGossipPex
  gossip'RxPex <- readRecv cntGossipPex
  logger InfoS "Gossip stats" LogGossip{..}
