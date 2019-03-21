{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Mock P2P
module Thundermint.P2P (
    startPeerDispatcher
  , LogGossip(..)
  , NetAddr(..)
  , netAddrToSockAddr
  , sockAddrToNetAddr
  , generatePeerId
  ) where

import Codec.Serialise
import Control.Applicative
import Control.Concurrent      ( ThreadId, myThreadId, killThread
                               , MVar, readMVar, newMVar)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Fail hiding (fail)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry           (recoverAll, exponentialBackoff, limitRetries, RetryPolicy)
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
import System.Random (newStdGen, randomIO, randomRIO)
import System.Random.Shuffle (shuffle')
import GHC.Generics  (Generic)


import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Blockchain.Internal.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Monitoring
import Thundermint.P2P.Network
import Thundermint.P2P.PeerState
import Thundermint.P2P.Types
import Thundermint.Store
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators
import Thundermint.Utils


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Messages which peers exchange with each other
--
--   FIXME: We don't have good way to prevent DoS by spamming too much
--          data
data GossipMsg alg a
  = GossipPreVote   !(Signed 'Unverified alg (Vote 'PreVote   alg a))
  | GossipPreCommit !(Signed 'Unverified alg (Vote 'PreCommit alg a))
  | GossipProposal  !(Signed 'Unverified alg (Proposal alg a))
  | GossipBlock     !(Block alg a)
  | GossipAnn       !(Announcement alg)
  | GossipTx        !(TX a)
  | GossipPex       !PexMessage
  deriving (Generic)
deriving instance (Show a, Show (TX a), Crypto alg) => Show (GossipMsg alg a)
instance (Serialise (TX a), Serialise a, Crypto alg) => Serialise (GossipMsg alg a)


-- | Peer exchage gossip sub-message
--
data PexMessage
  = PexMsgAskForMorePeers
  -- ^ Peer need yet connections to peers
  | PexMsgMorePeers ![NetAddr]
  -- ^ Some addresses of other connected peers
  | PexPing
  -- ^ Message to estimate connection speed between peers
  | PexPong
  -- ^ Answer for Ping
  deriving (Show, Generic)

instance Serialise PexMessage


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
data PeerChans m alg a = PeerChans
  { peerChanTx      :: !(TChan (Announcement alg))
    -- ^ Broadcast channel for outgoing messages
  , peerChanPex     :: !(TChan PexMessage)
    -- ^ Broadcast channel for outgoing PEX messages
  , peerChanPexNewAddresses :: !(TChan [NetAddr])
    -- ^ Channel for new addreses
  , peerChanRx      :: !(MessageRx 'Unverified alg a -> STM ())
    -- ^ STM action for sending message to main application
  , proposalStorage :: !(ProposalStorage 'RO m alg a)
    -- ^ Read only access to storage of proposals
  , consensusState  :: !(STM (Maybe (Height, TMState alg a)))
    -- ^ Read only access to current state of consensus state machine
  , p2pConfig       :: !NetworkCfg

  , cntGossipPrevote   :: !Counter
  , cntGossipPrecommit :: !Counter
  , cntGossipBlocks    :: !Counter
  , cntGossipProposals :: !Counter
  , cntGossipTx        :: !Counter
  , cntGossipPex       :: !Counter
  }

-- | Counter for counting send/receive event
data Counter = Counter !(MVar Int) !(MVar Int)

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
  :: ( MonadMask m, MonadFork m, MonadLogger m, MonadTrace m, MonadReadDB m alg a, MonadTMMonitoring m
     , BlockData a,  Show a, Crypto alg, MonadFail m)
  => NetworkCfg
  -> NetworkAPI               -- ^ API for networking
  -> NetAddr                  -- ^ Current peer address
  -> [NetAddr]                -- ^ Set of initial addresses to connect
  -> AppChans m alg a         -- ^ Channels for communication with main application
  -> Mempool m alg (TX a)
  -> m ()
startPeerDispatcher p2pConfig net peerAddr addrs AppChans{..} mempool = logOnException $ do
  let PeerInfo peerId _ _ = ourPeerInfo net
  logger InfoS ("Starting peer dispatcher: addrs = " <> showLS addrs <> ", PeerId = " <> showLS peerId) ()
  trace TeNodeStarted
  peerRegistry       <- newPeerRegistry peerId
  peerChanPex        <- liftIO newBroadcastTChanIO
  peerChanPexNewAddresses <- liftIO newTChanIO
  cntGossipPrevote   <- newCounter
  cntGossipPrecommit <- newCounter
  cntGossipProposals <- newCounter
  cntGossipBlocks    <- newCounter
  cntGossipTx        <- newCounter
  cntGossipPex       <- newCounter
  let peerCh = PeerChans { peerChanTx      = appChanTx
                         , peerChanPex     = peerChanPex
                         , peerChanRx      = writeTBQueue appChanRx
                         , proposalStorage = makeReadOnlyPS appPropStorage
                         , consensusState  = readTVar appTMState
                         , ..
                         }
  -- Accepting connection is managed by separate linked thread and
  -- this thread manages initiating connections
  flip finally (uninterruptibleMask_ $ reapPeers peerRegistry) $ runConcurrently
    [ acceptLoop p2pConfig peerAddr net peerCh mempool peerRegistry
     -- FIXME: we should manage requests for peers and connecting to
     --        new peers here
    , do waitSec 0.1
         forM_ addrs $ \a ->
             connectPeerTo p2pConfig peerAddr net a peerCh mempool peerRegistry
         forever $ waitSec 0.1
    -- Peer connection monitor
    , descendNamespace "PEX" $
      peerPexMonitor p2pConfig peerAddr net peerCh mempool peerRegistry
    -- Peer connection capacity monitor for debug purpose
    , descendNamespace "PEX" $
      peerPexCapacityDebugMonitor peerRegistry
    -- Peer new addreses capacity monitor
    , descendNamespace "PEX" $
      peerPexKnownCapacityMonitor peerAddr peerCh peerRegistry (pexMinKnownConnections p2pConfig) (pexMaxKnownConnections p2pConfig)
    -- Listen for new raw node addresses; normalize it and put into prKnownAddreses
    , descendNamespace "PEX" $
      peerPexNewAddressMonitor peerChanPexNewAddresses peerRegistry net
    -- Output gossip statistics to
    , forever $ do
        logGossip peerCh
        waitSec 1.0
    ]


-- | Generate "unique" peer id for current session.
--
generatePeerId :: (MonadIO m) => m PeerId
generatePeerId = liftIO randomIO


data ConnectMode = CmAccept !PeerId
                 | CmConnect
                 deriving Show


retryPolicy :: NetworkCfg -> RetryPolicy
retryPolicy NetworkCfg{..} = exponentialBackoff (reconnectionDelay * 1000)
                          <> limitRetries reconnectionRetries


-- Thread which accepts connections from remote nodes
acceptLoop
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadTrace m, MonadReadDB m alg a
     , BlockData a, Show a, Crypto alg, MonadFail m)
  => NetworkCfg
  -> NetAddr
  -> NetworkAPI
  -> PeerChans m alg a
  -> Mempool m alg (TX a)
  -> PeerRegistry
  -> m ()
acceptLoop cfg peerAddr NetworkAPI{..} peerCh mempool peerRegistry = do
  logger InfoS "Starting accept loop" ()
  recoverAll (retryPolicy cfg) $ const $ logOnException $
    bracket listenOn fst $ \(_,accept) -> forever $
      -- We accept connection, create new thread and put it into
      -- registry. If we already have connection from that peer we close
      -- connection immediately
      mask $ \restore -> do
        (conn, addr') <- accept
        void $ flip forkFinally (const $ close conn) $ restore $ do
          let peerInfo = connectedPeer conn
          logger InfoS ("Accept connection " <> showLS addr' <> ", peer info " <> showLS peerInfo) ("addr" `sl` show addr')
          let otherPeerId   = piPeerId   peerInfo
              otherPeerPort = piPeerPort peerInfo
              addr = normalizeNodeAddress addr' (Just $ fromIntegral otherPeerPort)
          trace $ TeNodeOtherTryConnect (show addr)
          logger DebugS "PreAccepted connection"
                (  sl "addr"     (show addr )
                <> sl "addr0"    (show addr')
                <> sl "peerId"   otherPeerId
                <> sl "peerPort" otherPeerPort
                )
          if otherPeerId == prPeerId peerRegistry then do
            logger DebugS "Self connection detected. Close connection" ()
          else do
            catch (withPeer peerRegistry addr (CmAccept otherPeerId) $ do
                  logger InfoS "Accepted connection" ("addr" `sl` show addr)
                  trace $ TeNodeOtherConnected (show addr)
                  descendNamespace (T.pack (show addr))
                    $ startPeer peerAddr addr peerCh conn peerRegistry mempool
                  ) (\e -> logger InfoS ("withPeer has thrown " <> showLS (e :: SomeException)) ())


-- Initiate connection to remote host and register peer
connectPeerTo
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadTrace m, MonadReadDB m alg a
     , BlockData a, Show a, Crypto alg, MonadFail m
     )
  => NetworkCfg
  -> NetAddr
  -> NetworkAPI
  -> NetAddr
  -> PeerChans m alg a
  -> Mempool m alg (TX a)
  -> PeerRegistry
  -> m ()
connectPeerTo cfg peerAddr NetworkAPI{..} addr peerCh mempool peerRegistry =
  -- Igrnore all exceptions to prevent apparing of error messages in stderr/stdout.
  void . flip forkFinally (const $ return ()) $
    recoverAll (retryPolicy cfg) $ const $ logOnException $ do
      logger InfoS "Connecting to" (sl "addr" (show addr))
      trace (TeNodeConnectingTo (show addr))
      -- TODO : what first? "connection" or "withPeer" ?
      bracket (connect addr) (\c -> logClose >> close c) $ \conn -> do
        withPeer peerRegistry addr CmConnect $ do
            logger InfoS "Successfully connected to" (sl "addr" (show addr))
            descendNamespace (T.pack (show addr))
              $ startPeer peerAddr addr peerCh conn peerRegistry mempool
        logClose
  where
    logClose = logger InfoS "Connection closed" (sl "addr" (show addr))


-- Set of currently running peers
data PeerRegistry = PeerRegistry
    { prTidMap        :: !(TVar (Map ThreadId NetAddr))
      -- ^ Threads that process connection to address
    , prConnected     :: !(TVar (Set NetAddr))
      -- ^ Connected addresses
    , prKnownAddreses :: !(TVar (Set NetAddr))
      -- ^ New addresses to connect
    , prPeerId        :: !PeerId
      -- ^ Unique peer id for controlling simultaneous connections
    , prIsActive      :: !(TVar Bool)
      -- ^ `False` when close all connections
    }


-- | Create new empty and active registry
newPeerRegistry :: MonadIO m => PeerId -> m PeerRegistry
newPeerRegistry pid = PeerRegistry
               <$> liftIO (newTVarIO Map.empty)
               <*> liftIO (newTVarIO Set.empty)
               <*> liftIO (newTVarIO Set.empty)
               <*> return pid
               <*> liftIO (newTVarIO True)

-- | Register peer using current thread ID. If we already have
--   registered peer with given address do nothing
--   NOTE: we need to track activity of registry to avoid possibility of
--         successful registration after call to reapPeers
withPeer :: (MonadMask m, MonadLogger m, MonadIO m, MonadTrace m)
         => PeerRegistry -> NetAddr -> ConnectMode -> m () -> m ()
withPeer PeerRegistry{..} addr connMode action = do
  tid <- liftIO myThreadId
  logger DebugS ("withPeer: addr = " <> showLS addr <> ": peerId = " <> showLS prPeerId <> ", connMode = " <> showLS connMode <> ", tid: " <> showLS tid) ()
  -- NOTE: we need uninterruptibleMask since we STM operation are
  --       blocking and they must not be interrupted
  uninterruptibleMask $ \restore -> do
    r@(ok, addrs) <- liftIO $ atomically $ registerPeer tid
    logger DebugS ("withPeer: result: " <> showLS r) ()
    when ok $
        restore (tracePRChange addrs >> action)
        `finally`
        (logUnregister >> liftIO (atomically (unregisterPeer tid)) >>= tracePRChange)
  where
    tracePRChange addrs = trace $ TePeerRegistryChanged (Set.map show addrs)
    -- Add peer to registry and return whether it was success
    registerPeer tid = readTVar prIsActive >>= \case
      False -> return (False, Set.empty)
      True  -> do
        addrs <- readTVar prConnected
        if addr `Set.member` addrs then
          case connMode of
            CmConnect -> return (False, addrs)
            CmAccept otherPeerId ->
              -- Something terrible happened: mutual connection!
              -- So we compare peerId-s: lesser let greater have connection.
              if prPeerId > otherPeerId then
                -- Wait for closing connection on other side
                -- and release addr from addrs.
                retry
              else
                -- Deny connection on this size
                -- (for releasing addr from addrs on other side).
                return (False, addrs)
        else do
          modifyTVar' prTidMap    $ Map.insert tid addr
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
    logUnregister = logger DebugS ("withPeer: unregister " <> showLS addr) ()


-- Kill all registered threads
reapPeers :: MonadIO m => PeerRegistry -> m ()
reapPeers PeerRegistry{..} = liftIO $ do
  tids <- atomically $ do
    writeTVar prIsActive False
    readTVar prTidMap
  mapM_ killThread $ Map.keys tids

----------------------------------------------------------------
-- Peer exchange
----------------------------------------------------------------


ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM predicate thenAct elseAct =
    predicate >>= \case
        True  -> thenAct
        False -> elseAct


whenM :: (Monad m) => m Bool -> m () -> m ()
whenM predicate act = ifM predicate act (return ())


peerPexNewAddressMonitor
  :: ( MonadIO m, MonadThrow m, MonadMask m, MonadFail m)
  => TChan [NetAddr]
  -> PeerRegistry
  -> NetworkAPI
  -> m ()
peerPexNewAddressMonitor peerChanPexNewAddresses PeerRegistry{..} NetworkAPI{..} = forever $ do
  addrs' <- liftIO $ atomically $ readTChan peerChanPexNewAddresses
  addrs  <- filterOutOwnAddresses $ Set.fromList $ map (`normalizeNodeAddress` Nothing) addrs'
  liftIO $ atomically $ modifyTVar' prKnownAddreses (`Set.union` addrs)


peerPexKnownCapacityMonitor
  :: ( MonadIO m, MonadLogger m
     , Serialise a, Show a, Crypto alg)
  => NetAddr
  -> PeerChans m alg a
  -> PeerRegistry
  -> Int
  -> Int
  -> m ()
peerPexKnownCapacityMonitor _peerAddr PeerChans{..} PeerRegistry{..} minKnownConnections _maxKnownConnections = do
    logger InfoS "Start PEX known capacity monitor" ()
    liftIO $ atomically $ readTVar prConnected >>= (check . not . Set.null) -- wait until some initial peers connect
    logger DebugS "Some nodes connected" ()
    forever $ do
        currentKnowns <- liftIO (readTVarIO prKnownAddreses)
        if Set.size currentKnowns < minKnownConnections then do
            logger DebugS ("Too few known (" <> showLS (Set.size currentKnowns) <> ":" <> showLS currentKnowns <> ") conns (need "<>showLS minKnownConnections<>"); ask for more known connections") ()
            -- TODO firstly ask only last peers
            liftIO $ atomically $ writeTChan peerChanPex PexMsgAskForMorePeers
            waitSec 1.0 -- TODO wait for new connections OR timeout (see https://stackoverflow.com/questions/22171895/using-tchan-with-timeout)
        else do
            logger DebugS ("Full of knowns conns (" <> showLS (Set.size currentKnowns) <> ")") ()
            waitSec 10.0


peerPexMonitor
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadTrace m, MonadReadDB m alg a
     , BlockData a, Show a, Crypto alg, MonadFail m)
  => NetworkCfg
  -> NetAddr -- ^ Current peer address for logging purpose
  -> NetworkAPI
  -> PeerChans m alg a
  -> Mempool m alg (TX a)
  -> PeerRegistry
  -> m ()
peerPexMonitor cfg peerAddr net peerCh mempool peerRegistry@PeerRegistry{..} = do
    logger InfoS "Start PEX monitor" ()
    locAddrs <- getLocalAddresses
    logger DebugS ("Local addresses: " <> showLS locAddrs) ()
    liftIO $ atomically $ readTVar prConnected >>= (check . not . Set.null) -- wait until some initial peers connect
    logger DebugS "Some nodes connected" ()
    fix $ \nextLoop ->
        whenM (liftIO $ readTVarIO prIsActive) $ do
            conns <- liftIO $ readTVarIO prConnected
            let sizeConns = Set.size conns
            if sizeConns < pexMinConnections cfg then do
                logger DebugS ("Too few (" <> showLS (Set.size conns) <> " : " <> showLS conns <> ") connections") ()
                knowns' <- liftIO $ readTVarIO prKnownAddreses
                let conns' = Set.map (flip (normalizeNodeAddress net) Nothing) conns -- TODO нужно ли тут normalize?
                    knowns = knowns' Set.\\ conns'
                if Set.null knowns then do
                    logger WarningS ("Too few (" <> showLS (Set.size conns) <> ") connections and don't know other nodes!") ()
                    waitSec 0.1
                else do
                    logger DebugS ("New peers: " <> showLS knowns) ()
                    rndGen <- liftIO newStdGen
                    let randKnowns = take (pexMaxConnections cfg - sizeConns)
                                   $ shuffle' (Set.toList knowns) (Set.size knowns) rndGen
                    logger DebugS ("New rand knowns: " <> showLS randKnowns) ()
                    forM_ randKnowns $ \addr -> connectPeerTo cfg peerAddr net addr peerCh mempool peerRegistry
                    waitSec 1.0
            else do
                logger InfoS ("Full of connections (" <> showLS (Set.size conns) <> " : " <>  showLS conns <> ")") ()
                waitSec 10.0
            nextLoop


-- | Watch number of connections and report it to monitoring system
--
peerPexCapacityDebugMonitor
  :: (MonadIO m, MonadTMMonitoring m)
  => PeerRegistry
  -> m ()
peerPexCapacityDebugMonitor PeerRegistry{..} =
  fix $ \loop -> do
    liftIO (readTVarIO prIsActive) >>= \case
      True  -> usingGauge prometheusNumPeers . Set.size =<< liftIO (readTVarIO prConnected)
      False -> usingGauge prometheusNumPeers 0
    waitSec 1.0
    loop


peerGossipPeerExchange
  :: ( MonadIO m, MonadFork m, MonadMask m, MonadLogger m
     , Show a, Serialise a, Crypto alg)
  => NetAddr
  -> PeerChans m alg a
  -> PeerRegistry
  -> TChan PexMessage
  -> TBQueue (GossipMsg alg a)
  -> m ()
peerGossipPeerExchange _peerAddr PeerChans{..} PeerRegistry{prConnected,prIsActive} pexCh gossipCh = forever $
    liftIO (atomically $ readTChan pexCh) >>= \case
        PexMsgAskForMorePeers -> sendPeers
        PexMsgMorePeers addrs -> connectToAddrs addrs
        PexPing               -> ping
        PexPong               -> pong
  where
    sendPeers = do
        addrList' <- Set.toList <$> liftIO (readTVarIO prConnected)
        logger DebugS ("peerGossipPeerExchange: someone asks for other peers: we answer " <> showLS addrList') ()
        isSomethingSent <- liftIO $ atomically $
            readTVar prIsActive >>= \case
                False -> return False
                True -> do
                    addrList <- Set.toList <$> readTVar prConnected
                    if null addrList then
                        return False
                    else do
                        writeTBQueue gossipCh (GossipPex (PexMsgMorePeers addrList)) -- TODO send only for requesting node!!!
                        return True
        when isSomethingSent $ tickSend cntGossipPex
    connectToAddrs addrs = do
        logger DebugS ("peerGossipPeerExchange: some address received: " <> showLS addrs) ()
        liftIO $ atomically $ writeTChan peerChanPexNewAddresses addrs
    ping = do
        liftIO $ atomically $ writeTBQueue gossipCh (GossipPex PexPong)
        tickSend cntGossipPex
    pong = return ()


----------------------------------------------------------------
-- Peer
----------------------------------------------------------------

-- | Start interactions with peer. At this point connection is already
--   established and peer is registered.
startPeer
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadReadDB m alg a
     , Show a, BlockData a, Crypto alg, MonadFail m)
  => NetAddr
  -> NetAddr
  -> PeerChans m alg a       -- ^ Communication with main application
                             --   and peer dispatcher
  -> P2PConnection           -- ^ Functions for interaction with network
  -> PeerRegistry
  -> Mempool m alg (TX a)
  -> m ()
startPeer peerAddrFrom peerAddrTo peerCh@PeerChans{..} conn peerRegistry mempool = logOnException $ do
  logger InfoS "Starting peer" ()
  liftIO $ atomically $ writeTChan peerChanPexNewAddresses [peerAddrTo]
  peerSt   <- newPeerStateObj proposalStorage
  gossipCh <- liftIO (newTBQueueIO 10)
  pexCh    <- liftIO newTChanIO
  cursor   <- getMempoolCursor mempool
  runConcurrently
    [ descendNamespace "gspV" $ peerGossipVotes         peerSt peerCh gossipCh
    , descendNamespace "gspB" $ peerGossipBlocks        peerSt peerCh gossipCh
    , descendNamespace "gspM" $ peerGossipMempool       peerSt peerCh p2pConfig gossipCh cursor
    , descendNamespace "PEX"  $ peerGossipPeerExchange  peerAddrFrom peerCh peerRegistry pexCh gossipCh
    , descendNamespace "send" $ peerSend                peerAddrFrom peerAddrTo peerSt peerCh gossipCh conn
    , descendNamespace "recv" $ peerReceive             peerSt peerCh pexCh conn cursor
    , peerGossipAnnounce peerCh gossipCh
    ]
  logger InfoS "Stopping peer" ()


-- | Gossip votes with given peer
peerGossipVotes
  :: (MonadReadDB m alg a, MonadFork m, MonadMask m, MonadLogger m, Crypto alg, Serialise a)
  => PeerStateObj m alg a         -- ^ Current state of peer
  -> PeerChans m alg a            -- ^ Read-only access to
  -> TBQueue (GossipMsg alg a)
  -> m x
peerGossipVotes peerObj PeerChans{..} gossipCh = logOnException $ do
  logger InfoS "Starting routine for gossiping votes" ()
  forever $ do
    bchH <- queryRO blockchainHeight
    peer <- getPeerState peerObj
    case peer of
      --
      Lagging p -> do
        mcmt <- case lagPeerStep p of
          FullStep peerH _ _
            | peerH == bchH -> queryRO $ retrieveLocalCommit peerH
            | otherwise     -> queryRO $ retrieveCommit      peerH
        --
        case mcmt of
         Just cmt -> do
           let cmtVotes  = Map.fromList [ (signedAddr v, unverifySignature v)
                                        | v <- commitPrecommits cmt ]
               -- FIXME: inefficient
           let toSet = Set.fromList
                     . map (fingerprint . validatorPubKey)
                     . mapMaybe (validatorByIndex (lagPeerValidators p))
                     . getValidatorIntSet
           let peerVotes = Map.fromSet (const ())
                         $ toSet (lagPeerPrecommits p)
               unknown   = Map.difference cmtVotes peerVotes
           case Map.size unknown of
             0 -> return ()
             n -> do i <- liftIO $ randomRIO (0,n-1)
                     let vote = unverifySignature $ toList unknown !! i
                     addPrecommit peerObj vote
                     liftIO $ atomically $ writeTBQueue gossipCh $ GossipPreCommit vote
                     tickSend cntGossipPrecommit
         Nothing -> return ()
      --
      Current p -> liftIO (atomically consensusState) >>= \case
        Nothing                       -> return ()
        Just (h',_) | h' /= succ bchH -> return ()
        Just (_,tm)                   -> do
          let FullStep _ r _ = peerStep p
              doGosip        = liftIO . atomically . writeTBQueue gossipCh
          -- FIXME: poor performance. Avoid going through map!
          let toSet = Set.fromList
                    . map (fingerprint . validatorPubKey)
                    . mapMaybe (validatorByIndex (peerValidators p))
                    . getValidatorIntSet

          -- Send proposals
          case () of
            _| not $ r `Set.member` peerProposals p
             , Just pr <- r `Map.lookup` smProposals tm
               -> do let prop = signedValue pr
                     addProposal peerObj (propHeight prop) (propRound prop)
                     doGosip $ GossipProposal $ unverifySignature pr
                     tickSend cntGossipProposals
             | otherwise -> return ()
          -- Send prevotes
          case () of
            _| Just localPV <- Map.lookup r $ toPlainMap $ smPrevotesSet tm
             , unknown      <- Map.difference localPV peerPV
             , not (Map.null unknown)
               -> do let n = Map.size unknown
                     i <- liftIO $ randomRIO (0,n-1)
                     let vote = unverifySignature $ toList unknown !! i
                     addPrevote peerObj vote
                     doGosip $ GossipPreVote vote
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
                     let vote = unverifySignature $ toList unknown !! i
                     addPrecommit peerObj vote
                     doGosip $ GossipPreCommit vote
                     tickSend cntGossipPrecommit
             | otherwise -> return ()
             where
               peerPC = maybe Map.empty (Map.fromSet (const ()) . toSet)
                      $ Map.lookup r $ peerPrecommits p
      Ahead   _ -> return ()
      Unknown   -> return ()
    waitSec (0.001 * fromIntegral (gossipDelayVotes p2pConfig))

peerGossipMempool
  :: ( MonadIO m, MonadFork m, MonadMask m, MonadLogger m
     )
  => PeerStateObj m alg a
  -> PeerChans m alg a
  -> NetworkCfg
  -> TBQueue (GossipMsg alg a)
  -> MempoolCursor m alg (TX a)
  -> m x
peerGossipMempool peerObj PeerChans{..} config gossipCh MempoolCursor{..} = logOnException $ do
  logger InfoS "Starting routine for gossiping transactions" ()
  forever $ do
    getPeerState peerObj >>= \case
      Current{} -> gossipTx
      Ahead{}   -> gossipTx
      _         -> return ()
    waitSec (0.001 * fromIntegral (gossipDelayMempool config))
  where
    gossipTx = advanceCursor >>= \case
      Just tx -> do liftIO $ atomically $ writeTBQueue gossipCh $ GossipTx tx
                    tickSend cntGossipTx
      Nothing -> return ()

-- | Gossip blocks with given peer
peerGossipBlocks
  :: (MonadReadDB m alg a, MonadFork m, MonadMask m, MonadLogger m, Serialise a, Crypto alg, MonadFail m)
  => PeerStateObj m alg a       -- ^ Current state of peer
  -> PeerChans m alg a          -- ^ Read-only access to
  -> TBQueue (GossipMsg alg a)  -- ^ Network API
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
                Just b <- queryRO $ retrieveBlock h -- FIXME: Partiality
                liftIO $ atomically $ writeTBQueue gossipCh $ GossipBlock b
                tickSend cntGossipBlocks
        | otherwise -> return ()
      --
      Current p -> do
        let FullStep h r _ = peerStep p
        mbid <- retrievePropByR proposalStorage h r
        case () of
           -- Peer has proposal but not block
          _| Just (b,bid) <- mbid
           , r `Set.member` peerProposals p
           , not $ bid `Set.member` peerBlocks p
             -> do logger DebugS ("Gossip: " <> showLS bid) ()
                   liftIO $ atomically $ writeTBQueue gossipCh $ GossipBlock b
                   tickSend cntGossipBlocks
           --
           | otherwise -> return ()
      -- Nothing to do
      Ahead _ -> return ()
      Unknown -> return ()
    waitSec (0.001 * fromIntegral (gossipDelayBlocks p2pConfig))

-- | Routine for receiving messages from peer
peerReceive
  :: ( MonadReadDB m alg a, MonadFork m, MonadMask m, MonadLogger m, MonadFail m
     , Crypto alg, BlockData a)
  => PeerStateObj m alg a
  -> PeerChans m alg a
  -> TChan PexMessage
  -> P2PConnection
  -> MempoolCursor m alg (TX a)
  -> m ()
peerReceive peerSt PeerChans{..} peerExchangeCh P2PConnection{..} MempoolCursor{..} = logOnException $ do
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
          GossipTx tx       -> do void $ pushTransaction tx
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

-- Infrequently announce our current state. This is needed if node was
-- terminated when it got all necessary votes but don't have block
-- yet. On start it will quickly replay WAL, enter StepAwaitCommit and
-- will never change state and announce it.
peerGossipAnnounce
  :: (MonadIO m, MonadLogger m, MonadCatch m)
  => PeerChans m alg a
  -> TBQueue (GossipMsg alg a)
  -> m ()
peerGossipAnnounce PeerChans{..} gossipCh = logOnException $
  forever $ do
    liftIO $ atomically $ do
      st <- consensusState
      forM_ st $ \(h,TMState{smRound,smStep}) -> do
        writeTBQueue gossipCh $ GossipAnn $ AnnStep $ FullStep h smRound smStep
        case smStep of
          StepAwaitCommit r -> writeTBQueue gossipCh $ GossipAnn $ AnnHasProposal h r
          _                 -> return ()
    waitSec 10

---- | Dump GossipMsg without (Show) constraints
----
--showlessShowGossipMsg :: (Show addr) => GossipMsg addr alg a -> Katip.LogStr
--showlessShowGossipMsg (GossipPreVote _)   = "GossipPreVote ..."
--showlessShowGossipMsg (GossipPreCommit _) = "GossipPreCommit ..."
--showlessShowGossipMsg (GossipProposal _)  = "GossipProposal ..."
--showlessShowGossipMsg (GossipBlock _)     = "GossipBlock ..."
--showlessShowGossipMsg (GossipAnn _)       = "GossipAnn ..."
--showlessShowGossipMsg (GossipTx _)        = "GossipTx ..."
--showlessShowGossipMsg (GossipPex p)       = "GossipPex { " <> showLS p <> " }"


-- | Routine for actually sending data to peers
peerSend
  :: ( MonadReadDB m alg a, MonadFork m, MonadMask m, MonadLogger m, MonadFail m
     , Crypto alg, BlockData a)
  => NetAddr
  -> NetAddr
  -> PeerStateObj m alg a
  -> PeerChans m alg a
  -> TBQueue (GossipMsg alg a)
  -> P2PConnection
  -> m x
peerSend _peerAddrFrom peerAddrTo peerSt PeerChans{..} gossipCh P2PConnection{..} = logOnException $ do
  logger InfoS ("Starting routing for sending data to " <> showLS peerAddrTo) ()
  ownPeerChanTx  <- liftIO $ atomically $ dupTChan peerChanTx
  ownPeerChanPex <- liftIO $ atomically $ dupTChan peerChanPex
  forever $ do
    msg <- liftIO $ atomically $  fmap GossipAnn (readTChan ownPeerChanTx)
                              <|> readTBQueue gossipCh
                              <|> fmap GossipPex (readTChan ownPeerChanPex)
    -- logger InfoS ("Send to (" <> showLS peerAddrTo <> "): " <> showlessShowGossipMsg msg) ()
    send $ serialise msg
    -- Update state of peer when we advance to next height
    case msg of
      GossipBlock b                        -> addBlock peerSt b
      GossipAnn (AnnStep (FullStep h _ _)) -> advanceOurHeight peerSt h
      _                                    -> return ()


logGossip
  :: ( MonadIO m, MonadFork m, MonadMask m, MonadLogger m, MonadTMMonitoring m )
  => PeerChans m alg a
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
  --
  usingVector prometheusGossip ("TX","prevote")   gossip'TxPV
  usingVector prometheusGossip ("RX","prevote")   gossip'RxPV
  usingVector prometheusGossip ("TX","precommit") gossip'TxPC
  usingVector prometheusGossip ("RX","precommit") gossip'RxPC
  usingVector prometheusGossip ("TX","proposal")  gossip'TxP
  usingVector prometheusGossip ("RX","proposal")  gossip'RxP
  usingVector prometheusGossip ("TX","block")     gossip'TxB
  usingVector prometheusGossip ("RX","block")     gossip'RxB
  --
  logger InfoS "Gossip stats" LogGossip{..}
