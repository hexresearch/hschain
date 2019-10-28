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
import Control.Monad.Trans.Cont
import Prelude                hiding (round)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Retry          (RetryPolicy, exponentialBackoff, limitRetries, recoverAll)
import Data.Foldable          (asum)
import Data.Function          (fix)
import Katip                  (showLS, sl)
import System.Random          (newStdGen)
import System.Random.Shuffle  (shuffle')

import qualified Data.Set           as Set
import qualified Data.Text          as T

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Blockchain.Internal.Types
import HSChain.Control
import HSChain.Crypto
import HSChain.Debug.Trace
import HSChain.Logger
import HSChain.Monitoring
import HSChain.P2P.Network
import HSChain.P2P.Types
import HSChain.P2P.PeerState.Timer
import HSChain.P2P.PeerState.Types
import HSChain.Store
import HSChain.Types.Blockchain
import HSChain.Utils
import HSChain.P2P.Internal.PeerRegistry
import HSChain.P2P.Internal.Logging as Logging
import HSChain.P2P.Internal.Types

import HSChain.P2P.PeerState.Handle
  (Command(..), Event(..), wrap, UnknownState(..), Config(..), handler)

--
-- Connect/accept
--

retryPolicy :: NetworkCfg -> RetryPolicy
retryPolicy NetworkCfg{..} = exponentialBackoff (reconnectionDelay * 1000)
                          <> limitRetries reconnectionRetries
-- Thread which accepts connections from remote nodes
acceptLoop
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadTrace m, MonadReadDB m alg a
     , BlockData a, Crypto alg)
  => NetworkCfg
  -> NetworkAPI
  -> PeerChans m alg a
  -> Mempool m alg (TX a)
  -> PeerRegistry
  -> m ()
acceptLoop cfg NetworkAPI{..} peerCh mempool peerRegistry = do
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
          logger InfoS ("Accept connection " <> showLS addr' <> ", peer info " <> showLS peerInfo) (sl "addr" addr')
          let otherPeerId   = piPeerId   peerInfo
              otherPeerPort = piPeerPort peerInfo
              addr = normalizeNodeAddress addr' (Just $ fromIntegral otherPeerPort)
          trace $ TeNodeOtherTryConnect (show addr)
          logger DebugS "PreAccepted connection"
                (  sl "addr"     addr
                <> sl "addr0"    addr'
                <> sl "peerId"   otherPeerId
                <> sl "peerPort" otherPeerPort
                )
          if otherPeerId == prPeerId peerRegistry then
            logger DebugS "Self connection detected. Close connection" ()
          else
            catch (withPeer peerRegistry addr (CmAccept otherPeerId) $ do
                  logger InfoS "Accepted connection" (sl "addr" addr)
                  trace $ TeNodeOtherConnected (show addr)
                  startPeer addr peerCh conn peerRegistry mempool
                  ) (\e -> logger InfoS ("withPeer has thrown " <> showLS (e :: SomeException)) ())


-- Initiate connection to remote host and register peer
connectPeerTo
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadTrace m, MonadReadDB m alg a
     , BlockData a, Crypto alg
     )
  => NetworkCfg
  -> NetworkAPI
  -> NetAddr
  -> PeerChans m alg a
  -> Mempool m alg (TX a)
  -> PeerRegistry
  -> m ()
connectPeerTo cfg NetworkAPI{..} addr peerCh mempool peerRegistry =
  -- Igrnore all exceptions to prevent apparing of error messages in stderr/stdout.
  void . flip forkFinally (const $ return ()) $
    recoverAll (retryPolicy cfg) $ const $ logOnException $ do
      logger InfoS "Connecting to" (sl "addr" addr)
      trace (TeNodeConnectingTo (show addr))
      -- TODO : what first? "connection" or "withPeer" ?
      bracket (connect addr) (\c -> logClose >> close c) $ \conn -> do
        withPeer peerRegistry addr CmConnect $ do
            logger InfoS "Successfully connected to" (sl "addr" addr)
            startPeer addr peerCh conn peerRegistry mempool
        logClose
  where
    logClose = logger InfoS "Connection closed" (sl "addr" addr)

----------------------------------------------------------------
-- Peer
----------------------------------------------------------------
-- | Routine for receiving messages from peer
peerFSM
  :: ( MonadReadDB m alg a, MonadIO m, MonadMask m, MonadLogger m
     , Crypto alg, BlockData a)
  => PeerChans m alg a
  -> TChan PexMessage
  -> TBQueue (GossipMsg alg a)
  -> TChan (Event alg a)
  -> MempoolCursor m alg (TX a)
  -> m (State alg a)
peerFSM PeerChans{..} peerExchangeCh gossipCh recvCh cursor@MempoolCursor{..} = logOnException $ do
  logger InfoS "Starting routing for receiving messages" ()
  ownPeerChanTx <- atomicallyIO $ dupTChan peerChanTx
  chTimeout     <- liftIO newTQueueIO
  evalContT $ do
    linkedTimer (gossipDelayVotes   p2pConfig) chTimeout EVotesTimeout
    linkedTimer (gossipDelayMempool p2pConfig) chTimeout EMempoolTimeout
    linkedTimer (gossipDelayBlocks  p2pConfig) chTimeout EBlocksTimeout
    linkedTimer  10e3                          chTimeout EAnnounceTimeout
    lift $ iterateM (wrap UnknownState) $ \s -> do
      event <- atomicallyIO $ asum
        [ readTQueue chTimeout
        , readTChan recvCh
        , EAnnouncement <$> readTChan ownPeerChanTx
        ]
      (s', cmds) <- handler config s event
      forM_ cmds $ \case
        SendRX rx         -> atomicallyIO $ peerChanRx rx -- FIXME: tickRecv
        Push2Mempool tx   -> void $ pushTransaction tx
        SendPEX pexMsg    -> atomicallyIO $ writeTChan peerExchangeCh pexMsg
        Push2Gossip tx    -> atomicallyIO $ writeTBQueue gossipCh tx
      return s'
  where
    config = Config proposalStorage cursor consensusState gossipCnts


-- | Start interactions with peer. At this point connection is already
--   established and peer is registered.
startPeer
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadReadDB m alg a, MonadTrace m
     , BlockData a, Crypto alg)
  => NetAddr
  -> PeerChans m alg a       -- ^ Communication with main application
                             --   and peer dispatcher
  -> P2PConnection           -- ^ Functions for interaction with network
  -> PeerRegistry
  -> Mempool m alg (TX a)
  -> m ()
startPeer peerAddrTo peerCh@PeerChans{..} conn peerRegistry mempool = logOnException $
  descendNamespace (T.pack (show peerAddrTo)) $ logOnException $ do
    logger InfoS "Starting peer" ()
    atomicallyIO $ writeTChan peerChanPexNewAddresses [peerAddrTo]
    gossipCh <- liftIO (newTBQueueIO 10)
    pexCh    <- liftIO newTChanIO
    recvCh   <- liftIO newTChanIO
    cursor   <- getMempoolCursor mempool
    runConcurrently
      [ descendNamespace "recv" $ peerReceive             peerCh recvCh conn
      , descendNamespace "send" $ peerSend                peerCh gossipCh conn
      , descendNamespace "PEX"  $ peerGossipPeerExchange  peerCh peerRegistry pexCh gossipCh
      , descendNamespace "peerFSM" $ void $ peerFSM              peerCh pexCh gossipCh recvCh cursor
      ]
    logger InfoS "Stopping peer" ()


-- | Routine for receiving messages from peer
peerReceive
  :: ( MonadReadDB m alg a, MonadIO m, MonadMask m, MonadLogger m
     , Crypto alg, BlockData a)
  => PeerChans m alg a
  -> TChan (Event alg a)
  -> P2PConnection
  -> m ()
peerReceive PeerChans{..} recvCh P2PConnection{..} = logOnException $ do
  logger InfoS "Starting routing for receiving messages" ()
  fix $ \loop -> recv >>= \case -- TODO поменять fix на forever, т.к. в последних версиях base они одни и те же
    Nothing  -> logger InfoS "Peer stopping since socket is closed" ()
    Just bs  -> case deserialiseOrFail bs of
      Left  e   -> logger ErrorS ("Deserialization error: " <> showLS e) ()
      Right msg -> do
        atomicallyIO $ writeTChan recvCh (EGossip msg)
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
    atomicallyIO $ do
      st <- consensusState
      forM_ st $ \(h,TMState{smRound,smStep}) -> do
        writeTBQueue gossipCh $ GossipAnn $ AnnStep $ FullStep h smRound smStep
        case smStep of
          StepAwaitCommit r -> writeTBQueue gossipCh $ GossipAnn $ AnnHasProposal h r
          _                 -> return ()
    waitSec 10

-- | Routine for actually sending data to peers
peerSend
  :: ( MonadReadDB m alg a, MonadMask m, MonadIO m,  MonadLogger m
     , Crypto alg, BlockData a, CryptoHashable a)
  => PeerChans m alg a
  -> TBQueue (GossipMsg alg a)
  -> P2PConnection
  -> m x
peerSend PeerChans{..} gossipCh P2PConnection{..} = logOnException $ do
  logger InfoS "Starting routing for sending data" ()
  ownPeerChanPex <- atomicallyIO $ dupTChan peerChanPex
  forever $ do
    msg <- atomicallyIO $  readTBQueue gossipCh
                       <|> fmap GossipPex (readTChan ownPeerChanPex)
    send $ serialise msg

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
  :: (MonadIO m)
  => TChan [NetAddr]
  -> PeerRegistry
  -> NetworkAPI
  -> m ()
peerPexNewAddressMonitor peerChanPexNewAddresses PeerRegistry{..} NetworkAPI{..} = forever $ do
  addrs' <- atomicallyIO $ readTChan peerChanPexNewAddresses
  addrs  <- fmap Set.fromList $ filterOutOwnAddresses $ map (`normalizeNodeAddress` Nothing) addrs'
  atomicallyIO $ modifyTVar' prKnownAddreses (`Set.union` addrs)


peerPexKnownCapacityMonitor
  :: ( MonadIO m, MonadLogger m)
  => PeerChans m alg a
  -> PeerRegistry
  -> Int
  -> Int
  -> m ()
peerPexKnownCapacityMonitor PeerChans{..} PeerRegistry{..} minKnownConnections _maxKnownConnections = do
    logger InfoS "Start PEX known capacity monitor" ()
    atomicallyIO $ readTVar prConnected >>= (check . not . Set.null) -- wait until some initial peers connect
    logger DebugS "Some nodes connected" ()
    forever $ do
        currentKnowns <- liftIO (readTVarIO prKnownAddreses)
        if Set.size currentKnowns < minKnownConnections then do
            logger DebugS ("Too few known (" <> showLS (Set.size currentKnowns) <> ":" <> showLS currentKnowns <> ") conns (need "<>showLS minKnownConnections<>"); ask for more known connections") ()
            -- TODO firstly ask only last peers
            atomicallyIO $ writeTChan peerChanPex PexMsgAskForMorePeers
            waitSec 1.0 -- TODO wait for new connections OR timeout (see https://stackoverflow.com/questions/22171895/using-tchan-with-timeout)
        else do
            logger DebugS ("Full of knowns conns (" <> showLS (Set.size currentKnowns) <> ")") ()
            waitSec 10.0


peerPexMonitor
  :: ( MonadFork m, MonadMask m, MonadLogger m, MonadTrace m, MonadReadDB m alg a
     , BlockData a, Crypto alg)
  => NetworkCfg
  -> NetworkAPI
  -> PeerChans m alg a
  -> Mempool m alg (TX a)
  -> PeerRegistry
  -> m ()
peerPexMonitor cfg net peerCh mempool peerRegistry@PeerRegistry{..} = do
    logger InfoS "Start PEX monitor" ()
    locAddrs <- getLocalAddresses
    logger DebugS ("Local addresses: " <> showLS locAddrs) ()
    atomicallyIO $ readTVar prConnected >>= (check . not . Set.null) -- wait until some initial peers connect
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
                    forM_ randKnowns $ \addr -> connectPeerTo cfg net addr peerCh mempool peerRegistry
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
  :: ( MonadIO m, MonadFork m, MonadLogger m)
  => PeerChans m alg a
  -> PeerRegistry
  -> TChan PexMessage
  -> TBQueue (GossipMsg alg a)
  -> m ()
peerGossipPeerExchange PeerChans{..} PeerRegistry{prConnected,prIsActive} pexCh gossipCh = forever $
    atomicallyIO (readTChan pexCh) >>= \case
        PexMsgAskForMorePeers -> sendPeers
        PexMsgMorePeers addrs -> connectToAddrs addrs
        PexPing               -> ping
        PexPong               -> pong
  where
    sendPeers = do
        addrList' <- Set.toList <$> liftIO (readTVarIO prConnected)
        logger DebugS ("peerGossipPeerExchange: someone asks for other peers: we answer " <> showLS addrList') ()
        isSomethingSent <- atomicallyIO $
            readTVar prIsActive >>= \case
                False -> return False
                True -> do
                    addrList <- Set.toList <$> readTVar prConnected
                    if null addrList then
                        return False
                    else do
                        writeTBQueue gossipCh (GossipPex (PexMsgMorePeers addrList)) -- TODO send only for requesting node!!!
                        return True
        when isSomethingSent $ tickSend $ pex gossipCnts
    connectToAddrs addrs = do
        logger DebugS ("peerGossipPeerExchange: some address received: " <> showLS addrs) ()
        atomicallyIO $ writeTChan peerChanPexNewAddresses addrs
    ping = do
        atomicallyIO $ writeTBQueue gossipCh (GossipPex PexPong)
        tickSend $ pex gossipCnts
    pong = return ()
