{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HSChain.P2P.Internal
  ( module HSChain.P2P.Internal
  , module HSChain.P2P.Internal.PeerRegistry
  , module HSChain.P2P.Internal.Types
  ) where

import Codec.Serialise
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont hiding (reset)
import Prelude                hiding (round)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Retry          (RetryPolicy, exponentialBackoff, limitRetries, recoverAll)
import Data.Foldable          (asum)
import Data.Text              (Text)
import Data.Word
import Katip                  (sl)
import System.Random          (newStdGen)
import System.Random.Shuffle  (shuffle')

import qualified Data.Set           as Set
import qualified Data.Text          as T

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Logger
import HSChain.Monitoring
import HSChain.P2P.Network
import HSChain.P2P.Types
import HSChain.P2P.PeerState.Timer
import HSChain.P2P.PeerState.Types
import HSChain.Store
import HSChain.Types.Blockchain
import HSChain.P2P.Internal.PeerRegistry
import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Handle
import HSChain.Utils
import qualified HSChain.P2P.Network.IpAddresses as Ip

--
-- Connect/accept
--

retryPolicy :: NetworkCfg -> RetryPolicy
retryPolicy NetworkCfg{..} = exponentialBackoff (reconnectionDelay * 1000)
                          <> limitRetries reconnectionRetries
-- Thread which accepts connections from remote nodes
acceptLoop
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadReadDB m a, MonadTMMonitoring m
     , BlockData a)
  => NetworkCfg
  -> NetworkAPI
  -> PeerChans a
  -> Mempool m (Alg a) (TX a)
  -> m ()
acceptLoop cfg NetworkAPI{..} peerCh mempool = do
  logger InfoS "Starting accept loop" ()
  recoverAll (retryPolicy cfg) $ const $ logOnException $
    bracket listenOn fst $ \(_,accept) -> forever $ do
      -- We accept connection and create new thread which is manager
      -- by shepherd
      mask $ \restore -> do
        (conn, addr) <- accept
        newSheepFinally (peerShepherd peerCh)
          (restore $ peerThread conn addr)
          (close conn)
  where
    peerThread conn addr = logOnException $ do
      -- Expect GossipHello from peer
      GossipHello nonce port <- deserialise <$> recv conn
      -- Check nonce for self-connection and send reply
      isSelfConnection (peerNonceSet peerCh) nonce >>= \case
        True  -> error "Self-connection"
        False -> return ()
      send conn $ serialise $ GossipAck
      -- Handshake is complete. Accept connection
      withPeer (peerRegistry peerCh) (normalizeNodeAddress addr port) $
        startPeer addr peerCh conn mempool


-- Initiate connection to remote host and register peer
connectPeerTo
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadReadDB m a, MonadTMMonitoring m
     , BlockData a
     )
  => NetworkAPI
  -> NetAddr
  -> PeerChans a
  -> Mempool m (Alg a) (TX a)
  -> m ()
connectPeerTo NetworkAPI{..} addr peerCh mempool =
  -- Igrnore all exceptions to prevent apparing of error messages in stderr/stdout.
  newSheep (peerShepherd peerCh) $ logOnException $ do
    bracket (connect addr) close $ \conn -> do
      -- Perform handshake
      withGossipNonce (peerNonceSet peerCh) $ \nonce -> do
        send conn $ serialise $ GossipHello nonce (piPeerPort ourPeerInfo)
        GossipAck <- deserialise <$> recv conn
        return ()
      -- Start peer
      withPeer (peerRegistry peerCh) addr $ do
        startPeer addr peerCh conn mempool


----------------------------------------------------------------
-- Peer
----------------------------------------------------------------

-- | Start interactions with peer. At this point connection is already
--   established and peer is registered.
startPeer
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadReadDB m a, MonadTMMonitoring m
     , BlockData a)
  => NetAddr
  -> PeerChans a           -- ^ Communication with main application
                           --   and peer dispatcher
  -> P2PConnection         -- ^ Functions for interaction with network
  -> Mempool m (Alg a) (TX a)
  -> m ()
startPeer peerAddrTo peerCh@PeerChans{..} conn mempool = logOnException $
  descendNamespace (T.pack (show peerAddrTo)) $ logOnException $ do
    logger InfoS "Starting peer" ()
    atomicallyIO $ writeTChan peerChanPexNewAddresses [peerAddrTo]
    gossipCh <- liftIO (newTBQueueIO 10)
    recvCh   <- liftIO newTChanIO
    cursor   <- getMempoolCursor mempool
    runConcurrently
      [ descendNamespace "recv"    $ peerReceive   recvCh conn
      , descendNamespace "send"    $ peerSend      peerCh gossipCh conn
      , descendNamespace "FSM"     $ peerFSM       peerCh gossipCh recvCh cursor
      , descendNamespace "mempool" $ mempoolThread p2pConfig gossipCh cursor
      ]
    logger InfoS "Stopping peer" ()


-- | Routine for receiving messages from peer
peerFSM
  :: ( MonadReadDB m a, MonadIO m, MonadMask m, MonadLogger m
     , BlockData a)
  => PeerChans a
  -> TBQueue (GossipMsg a)
  -> TChan (GossipMsg a)
  -> MempoolCursor m (Alg a) (TX a)
  -> m ()
peerFSM peerCh@PeerChans{..} gossipCh recvCh MempoolCursor{..} = logOnException $ do
  logger InfoS "Starting routing for receiving messages" ()
  ownPeerChanTx <- atomicallyIO $ dupTChan peerChanTx
  chTimeout     <- liftIO newTQueueIO
  evalContT $ do
    linkedTimer (gossipDelayVotes   p2pConfig) chTimeout TimeoutProposal
    linkedTimer (gossipDelayVotes   p2pConfig) chTimeout TimeoutPrevote
    linkedTimer (gossipDelayVotes   p2pConfig) chTimeout TimeoutPrecommit
    linkedTimer (gossipDelayBlocks  p2pConfig) chTimeout TimeoutBlock
    linkedTimer  10e3                          chTimeout TimeoutAnnounce
    lift $ iterateM (wrap UnknownState) $ \s -> do
      (s', cmds)
        <- join
         $ atomicallyIO
         $ asum [ handlerTimeout config s <$> readTQueue chTimeout
                , handlerGossip  config s <$> readTChan  recvCh
                , handlerTx      config s <$> readTChan  ownPeerChanTx
                ]
      forM_ cmds $ \case
        SendRX rx       -> atomicallyIO $ peerChanRx rx
        Push2Mempool tx -> void $ pushTransaction tx
        Push2Gossip  tx -> atomicallyIO $ writeTBQueue gossipCh tx
        SendPEX pex     -> handlePexMessage peerCh gossipCh pex
      return s'
  where
    config = Config consensusState

handlePexMessage :: MonadIO m => PeerChans a -> TBQueue (GossipMsg a) -> PexMessage -> m ()
handlePexMessage PeerChans{..} gossipCh = \case
  -- Peer ask for more addresses. Reply with list of peers we're
  -- connected to. They're known good
  PexMsgAskForMorePeers -> do
    addrs <- Set.toList <$> connectedAddresses peerRegistry
    unless (null addrs) $
      atomicallyIO $ writeTBQueue gossipCh $ GossipPex $ PexMsgMorePeers addrs
  -- Forward message to main PEX engine
  PexMsgMorePeers addrs -> do
    atomicallyIO $ writeTChan peerChanPexNewAddresses addrs

-- | Very simple generator of mempool gossip
mempoolThread
  :: (MonadLogger m, MonadCatch m, MonadIO m)
  => NetworkCfg
  -> TBQueue (GossipMsg a)
  -> MempoolCursor m alg (TX a)
  -> m b
mempoolThread NetworkCfg{..} gossipCh MempoolCursor{..} =
  logOnException $ forever $ do
    waitMSec gossipDelayMempool
    mapM_ (atomicallyIO . writeTBQueue gossipCh . GossipTx)
      =<< advanceCursor


-- | Routine for receiving messages from peer
peerReceive
  :: ( MonadReadDB m a, MonadIO m, MonadMask m, MonadLogger m, MonadTMMonitoring m
     , BlockData a)
  => TChan (GossipMsg a)
  -> P2PConnection
  -> m ()
peerReceive recvCh P2PConnection{..} = logOnException $ do
  logger InfoS "Starting routing for receiving messages" ()
  forever $ do
    bs <- recv
    let msg = deserialise bs
    atomicallyIO $ writeTChan recvCh msg
    countGossip "RX" msg


-- | Routine for actually sending data to peers
peerSend
  :: ( MonadReadDB m a, MonadMask m, MonadIO m, MonadLogger m, MonadTMMonitoring m
     , BlockData a)
  => PeerChans a
  -> TBQueue (GossipMsg a)
  -> P2PConnection
  -> m x
peerSend PeerChans{..} gossipCh P2PConnection{..} = logOnException $ do
  logger InfoS "Starting routing for sending data" ()
  ownPeerChanPex <- atomicallyIO $ dupTChan peerChanPex
  forever $ do
    msg <- atomicallyIO $  readTBQueue gossipCh
                       <|> fmap GossipPex (readTChan ownPeerChanPex)
    send $ serialise msg
    countGossip "TX" msg

countGossip
  :: (MonadIO m, MonadTMMonitoring m)
  => Text -> GossipMsg a -> m ()
countGossip dir = \case
  GossipPreVote{}   -> tick "PV"
  GossipPreCommit{} -> tick "PC"
  GossipProposal{}  -> tick "Prop"
  GossipBlock{}     -> tick "Blk"
  GossipAnn{}       -> return ()
  GossipTx{}        -> tick "TX"
  GossipPex{}       -> tick "PEX"
  where
    tick l = usingVector prometheusGossip (dir,l)

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


-- | Events for PEX state machine
data PEXEvents
  = EPexNewAddrs [NetAddr]
  -- ^ triggers when new peers addresses come
  | EPexCapacity
  -- ^ triggers requesting of known peers when ones are not enought
  | EPexMonitor
  -- ^ triggers check of peers connections and opening of new connections
  -- to known peers when it is nesessary

pexFSM :: (MonadLogger m, MonadMask m, MonadTMMonitoring m,
           MonadFork m, MonadReadDB m a, BlockData a)
          => NetworkCfg
          -> NetworkAPI
          -> PeerChans a
          -> Mempool m (Alg a) (TX a)
          -> Int
          -> m b
pexFSM cfg net@NetworkAPI{..} peerCh@PeerChans{..} mempool minKnownConnections = do
  logger InfoS "Start PEX FSM" ()
  locAddrs <- getLocalAddresses
  logger DebugS "Local addresses: " $ sl "addr" locAddrs
  logger DebugS "Some nodes connected" ()
  capTO <- newTimerIO
  monTO <- newTimerIO
  reset capTO 1e3
  reset monTO 1e3
  forever $ do
      event <- atomicallyIO $ asum
        [ EPexNewAddrs <$> readTChan peerChanPexNewAddresses
        , EPexCapacity <$  await capTO
        , EPexMonitor  <$  await monTO
        ]
      case event of
        -- We got new addresses
        EPexNewAddrs addrs -> atomicallyIO
                            $ addAddresses peerRegistry
                            $ map Ip.normalizeNetAddr addrs
        -- Request peers if we don't have enough
        EPexCapacity -> do
          known <- knownAddresses peerRegistry
          if Set.size known < minKnownConnections then do
              atomicallyIO $ writeTChan peerChanPex PexMsgAskForMorePeers
              reset capTO 1e3
          else do
              reset capTO 10e3
        -- Check if we have enough active connections
        EPexMonitor -> do
              conns <- connectedAddresses peerRegistry
              let sizeConns = Set.size conns
              if sizeConns < pexMinConnections cfg then do
                  logger DebugS "Too few connections" $ sl "connections" conns
                  knowns' <- knownAddresses peerRegistry
                  let knowns = knowns' Set.\\ conns
                  if Set.null knowns then do
                      reset monTO 0.1e3
                  else do
                      logger DebugS "New peers: " $ sl "peers" knowns
                      rndGen <- liftIO newStdGen
                      let randKnowns = take (pexMaxConnections cfg - sizeConns)
                                     $ shuffle' (Set.toList knowns) (Set.size knowns) rndGen
                      logger DebugS "New rand knowns: " $ sl "peers" randKnowns
                      forM_ randKnowns $ \addr -> connectPeerTo net addr peerCh mempool
                      reset monTO 1e3
              else do
                  logger InfoS "Full of connections" $ sl "connections" conns
                  reset monTO 10e3

pexMonitoring :: (MonadTMMonitoring m, MonadIO m) => PeerRegistry -> m a
pexMonitoring peerRegistry = forever $ do
  usingGauge prometheusNumPeers   . Set.size =<< connectedAddresses peerRegistry
  usingGauge prometheusKnownAddrs . Set.size =<< knownAddresses     peerRegistry
  waitSec 10

normalizeNodeAddress :: NetAddr -> Word16 -> NetAddr
normalizeNodeAddress = flip setPort . Ip.normalizeNetAddr
  where
    setPort port (NetAddrV4 ha _) = NetAddrV4 ha $ fromIntegral port
    setPort port (NetAddrV6 ha _) = NetAddrV6 ha $ fromIntegral port
