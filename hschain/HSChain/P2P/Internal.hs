{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module HSChain.P2P.Internal
  ( module HSChain.P2P.Internal
  , module HSChain.P2P.Internal.PeerRegistry
  , module HSChain.P2P.Internal.Types
  ) where

import Codec.Serialise
import Control.Applicative
import Control.Concurrent.STM
import Control.Exception      (AsyncException(..))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont hiding (reset)
import Prelude                hiding (round)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Retry          (RetryPolicy, exponentialBackoff, limitRetries, recoverAll)
import Data.Foldable          (asum)
import Data.Set               ((\\))
import Data.Text              (Text)
import Data.Word
import Katip                  (sl)
import System.Random          (newStdGen)
import System.Random.Shuffle  (shuffle')

import qualified Data.Set           as Set
import qualified Data.Text          as T

import HSChain.Control.Class
import HSChain.Control.Delay
import HSChain.Control.Shepherd
import HSChain.Control.Util
import HSChain.Control.Channels
import HSChain.Crypto (Hashed)
import HSChain.Internal.Types.Config
import HSChain.Internal.Types.Consensus
import HSChain.Logger
import HSChain.Mempool
import HSChain.Monitoring
import HSChain.Network.Mock (MockNetError)
import HSChain.Network.Types
import HSChain.P2P.Internal.PeerRegistry
import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Handle
import HSChain.P2P.PeerState.Timer
import HSChain.Store
import HSChain.Types.Blockchain
import qualified HSChain.Network.IpAddresses as Ip

--
-- Connect/accept
--

retryPolicy :: NetworkCfg app -> RetryPolicy
retryPolicy NetworkCfg{..} = exponentialBackoff (reconnectionDelay * 1000)
                          <> limitRetries reconnectionRetries
-- Thread which accepts connections from remote nodes
acceptLoop
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadReadDB m, MonadCached a m, MonadTMMonitoring m
     , BlockData a, a ~ BlockType view)
  => NetworkCfg app
  -> NetworkAPI
  -> PeerChans view
  -> Mempool (Hashed (Alg a) (TX a)) (TX a)
  -> m ()
acceptLoop cfg NetworkAPI{..} peerCh mempool = do
  logger DebugS "Starting accept loop" ()
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
    peerThread conn addr = convertNetException $ logOnException $ do
      logger DebugS "Preacceped peer" (sl "addr" addr)
      -- Expect GossipHello from peer
      GossipHello nonce port <- deserialise <$> recv conn
      let normAddr = normalizeNodeAddress addr port
      -- Check nonce for self-connection and send reply
      isSelfConnection (peerNonceSet peerCh) nonce >>= \case
        True  -> do addSelfAddress (peerRegistry peerCh) normAddr
                    throwM SelfConnection
        False -> return ()
      send conn $ serialise GossipAck
      logger DebugS "Accepted peer" (sl "addr" addr <> sl "norm" normAddr)
      -- Handshake is complete. Accept connection
      withPeer (peerRegistry peerCh) normAddr $
        startPeer addr peerCh conn mempool

-- Initiate connection to remote host and register peer
connectPeerTo
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadReadDB m, MonadCached a m, MonadTMMonitoring m
     , BlockData a, a ~ BlockType view
     )
  => NetworkAPI
  -> NetAddr
  -> PeerChans view
  -> Mempool (Hashed (Alg a) (TX a)) (TX a)
  -> m ()
connectPeerTo NetworkAPI{..} addr peerCh mempool =
  -- Ignore all exceptions to prevent apparing of error messages in stderr/stdout.
  newSheep (peerShepherd peerCh) $ convertNetException $ logOnException $ do
    logger DebugS "Connecting to" $ sl "addr" addr
    bracket (connect addr) close $ \conn -> do
      -- Perform handshake
      withGossipNonce (peerNonceSet peerCh) $ \nonce -> do
        send conn $ serialise $ GossipHello nonce $ fromIntegral listenPort
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
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadReadDB m, MonadCached a m, MonadTMMonitoring m
     , BlockData a, a ~ BlockType view)
  => NetAddr
  -> PeerChans view        -- ^ Communication with main application
                           --   and peer dispatcher
  -> P2PConnection         -- ^ Functions for interaction with network
  -> Mempool (Hashed (Alg a) (TX a)) (TX a)
  -> m ()
startPeer peerAddrTo peerCh@PeerChans{..} conn mempool = logOnException $
  descendNamespace (T.pack (show peerAddrTo)) $ logOnException $ do
    logger InfoS "Starting peer" ()
    gossipCh <- liftIO (newTBQueueIO 10)
    recvCh   <- liftIO newTChanIO
    cursor   <- getMempoolCursor $ mempoolHandle mempool
    runConcurrently
      [ descendNamespace "recv"    $ peerReceive   recvCh conn
      , descendNamespace "send"    $ peerSend      gossipCh conn
      , descendNamespace "FSM"     $ peerFSM       peerCh gossipCh recvCh cursor
      , descendNamespace "mempool" $ mempoolThread gossipCh mempool
      , descendNamespace "PEX"     $ pexCapacityThread peerRegistry p2pConfig gossipCh
      ]


-- | Routine for receiving messages from peer
peerFSM
  :: ( MonadReadDB m, MonadCached a m, MonadIO m, MonadMask m, MonadLogger m
     , BlockData a, a ~ BlockType view)
  => PeerChans view
  -> TBQueue (GossipMsg a)
  -> TChan (GossipMsg a)
  -> MempoolCursor (Hashed (Alg a) (TX a)) (TX a)
  -> m ()
peerFSM peerCh@PeerChans{..} gossipCh recvCh MempoolCursor{..} = logOnException $ do
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
        Push2Mempool tx -> void $ pushTxAsync tx
        Push2Gossip  tx -> atomicallyIO $ writeTBQueue gossipCh tx
        SendPEX pex     -> handlePexMessage peerCh gossipCh pex
      return s'
  where
    config = Config consensusState

handlePexMessage
  :: (MonadIO m, MonadLogger m)
  => PeerChans view -> TBQueue (GossipMsg a) -> PexMessage -> m ()
handlePexMessage PeerChans{..} gossipCh = \case
  -- Peer ask for more addresses. Reply with list of peers we're
  -- connected to. They're known good
  PexMsgAskForMorePeers -> do
    addrs <- Set.toList <$> connectedAddresses peerRegistry
    unless (null addrs) $
      atomicallyIO $ writeTBQueue gossipCh $ GossipPex $ PexMsgMorePeers addrs
  -- Forward message to main PEX engine
  PexMsgMorePeers addrs -> do
    let normAddrs = map Ip.normalizeNetAddr addrs
    logger DebugS "Adding addresses" (sl "addrs" normAddrs)
    atomicallyIO $ addAddresses peerRegistry normAddrs

-- | Very simple generator of mempool gossip
mempoolThread
  :: (MonadLogger m, MonadCatch m, MonadIO m)
  => TBQueue (GossipMsg a)
  -> Mempool (Hashed alg (TX a)) (TX a)
  -> m b
mempoolThread gossipCh Mempool{..} = do
  ch <- atomicallyIO makeNewTxBroadcast
  logOnException $ forever $ do
    atomicallyIO . writeTBQueue gossipCh . GossipTx =<< awaitIO ch 

-- | Thread for PEX interaction with peer.
--
--   NOTE: At the moment we only do asking about more addresses using
--         very simple logic. Implementing more complicated logic will
--         likely require turning this function into state machine and
pexCapacityThread
  :: (MonadIO m, MonadLogger m)
  => PeerRegistry
  -> NetworkCfg app
  -> TBQueue (GossipMsg a) -> m b
pexCapacityThread peerRegistry NetworkCfg{..} gossipCh = do
  -- Ask peer immediately for peers if we don't have enough
  atomicallyIO nonEnough
  askForMore
  forever $ do
    waitMSec pexAskPeersDelay
    atomicallyIO nonEnough
    askForMore
  where
    -- STM action to check whether we have enough peers.
    nonEnough = do addrs <- knownAddressesSTM peerRegistry
                   check $ Set.size addrs < pexMinKnownConnections
    --
    askForMore = do
      logger DebugS "Asking for more peers" ()
      atomicallyIO $ writeTBQueue gossipCh $ GossipPex PexMsgAskForMorePeers

-- | Routine for receiving messages from peer
peerReceive
  :: ( MonadReadDB m, MonadIO m, MonadMask m, MonadLogger m, MonadTMMonitoring m
     , BlockData a)
  => TChan (GossipMsg a)
  -> P2PConnection
  -> m ()
peerReceive recvCh P2PConnection{..} = logOnException $ do
  forever $ do
    bs <- recv
    let !msg = deserialise bs
    atomicallyIO $ writeTChan recvCh msg
    countGossip "RX" msg


-- | Routine for actually sending data to peers
peerSend
  :: ( MonadReadDB m, MonadMask m, MonadIO m, MonadLogger m, MonadTMMonitoring m
     , BlockData a)
  => TBQueue (GossipMsg a)
  -> P2PConnection
  -> m x
peerSend gossipCh P2PConnection{..} = logOnException $ do
  forever $ do
    msg <- atomicallyIO $ readTBQueue gossipCh
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

pexFSM
  :: (MonadLogger m, MonadMask m, MonadTMMonitoring m
     , MonadFork m, MonadReadDB m, MonadCached a m, BlockData a, a ~ BlockType view)
  => NetworkCfg app
  -> NetworkAPI
  -> PeerChans view
  -> Mempool (Hashed (Alg a) (TX a)) (TX a)
  -> m b
pexFSM cfg net peerCh@PeerChans{..} mempool = descendNamespace "PEX" $ do
  -- Start by connecting to peers
  forever $ do
    atomicallyIO nonEnough
    doConnect
    waitMSec $ pexConnectionDelay p2pConfig
  where
    -- Only succeed if we don't have enough connections
    nonEnough = do
      conns <- connectedAddressesSTM peerRegistry
      check $ Set.size conns < pexMinConnections p2pConfig
    -- Connect to random peer
    doConnect = do
      conns  <- connectedAddresses peerRegistry
      known  <- knownAddresses     peerRegistry
      self   <- selfAddresses      peerRegistry
      logger DebugS "Trying to connect"
        (  sl "conns" conns
        <> sl "known" known
        <> sl "self"  self
        )
      rndGen <- liftIO newStdGen
      let candidates = (known \\ conns) \\ self
      -- NOTE: shuffle hangs when given empty list as input
      when (Set.size candidates > 0) $ do
        let toConn = take (pexMaxConnections cfg - Set.size conns)
                   $ shuffle' (Set.toList candidates) (Set.size candidates) rndGen
        forM_ toConn $ \addr -> connectPeerTo net addr peerCh mempool

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

-- Convert network exceptions to ThreadKilled
--
-- This is hack to make running testing more pleasant. Tasty insists
-- on printing exceptions in threads failing due to them. But threads
-- failing due to network exception is pretty normal and expected
-- thing in our program. So we convert NetworkError/MockNetError to
-- ThreadKilled so it will dutyfully kill thread.
--
-- Loss of exception type is not important since we don't use it
-- anyway
convertNetException :: MonadCatch m => m () -> m ()
convertNetException = flip catches
  [ Handler (\(_ :: NetworkError) -> throwM ThreadKilled)
  , Handler (\(_ :: MockNetError) -> throwM ThreadKilled)
  ]
