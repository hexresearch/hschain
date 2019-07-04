{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NumDecimals          #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Thundermint.P2P.Internal
  ( module Thundermint.P2P.Internal
  , module Thundermint.P2P.Internal.PeerRegistry
  , module Thundermint.P2P.Internal.Types
  ) where

import Codec.Serialise
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Prelude                hiding (round)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Retry          (RetryPolicy, exponentialBackoff, limitRetries, recoverAll)
import Data.Foldable          (toList)
import Data.Function          (fix)
import Katip                  (showLS, sl)
import System.Random          (newStdGen, randomRIO)
import System.Random.Shuffle  (shuffle')

import qualified Data.IntMap.Strict as IMap
import qualified Data.IntSet        as ISet
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set
import qualified Data.Text          as T

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Blockchain.Internal.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Debug.Trace
import Thundermint.Exceptions
import Thundermint.Logger
import Thundermint.Monitoring
import Thundermint.P2P.Network
import Thundermint.P2P.PeerState
import Thundermint.P2P.Types
import Thundermint.Store
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators
import Thundermint.Utils

import Thundermint.P2P.Internal.PeerRegistry
import Thundermint.P2P.Internal.Logging as Logging
import Thundermint.P2P.Internal.Types

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
          if otherPeerId == prPeerId peerRegistry then
            logger DebugS "Self connection detected. Close connection" ()
          else
            catch (withPeer peerRegistry addr (CmAccept otherPeerId) $ do
                  logger InfoS "Accepted connection" ("addr" `sl` show addr)
                  trace $ TeNodeOtherConnected (show addr)
                  descendNamespace (T.pack (show addr))
                    $ startPeer addr peerCh conn peerRegistry mempool
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
      logger InfoS "Connecting to" (sl "addr" (show addr))
      trace (TeNodeConnectingTo (show addr))
      -- TODO : what first? "connection" or "withPeer" ?
      bracket (connect addr) (\c -> logClose >> close c) $ \conn -> do
        withPeer peerRegistry addr CmConnect $ do
            logger InfoS "Successfully connected to" (sl "addr" (show addr))
            descendNamespace (T.pack (show addr))
              $ startPeer addr peerCh conn peerRegistry mempool
        logClose
  where
    logClose = logger InfoS "Connection closed" (sl "addr" (show addr))

----------------------------------------------------------------
-- Peer
----------------------------------------------------------------

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
startPeer peerAddrTo peerCh@PeerChans{..} conn peerRegistry mempool = logOnException $ do
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
    , descendNamespace "recv" $ peerReceive             peerSt peerCh pexCh conn cursor
    , descendNamespace "send" $ peerSend                peerAddrTo peerSt peerCh gossipCh conn
    , descendNamespace "PEX"  $ peerGossipPeerExchange  peerCh peerRegistry pexCh gossipCh
    , peerGossipAnnounce peerCh gossipCh
    ]
  logger InfoS "Stopping peer" ()


-- | Gossip votes with given peer
peerGossipVotes
  :: ( MonadReadDB m alg a, MonadMask m, MonadIO m, MonadLogger m, MonadTrace m
     , Crypto alg, Serialise a)
  => PeerStateObj m alg a         -- ^ Current state of peer
  -> PeerChans    m alg a         -- ^ Read-only access to
  -> TBQueue (GossipMsg alg a)
  -> m ()
peerGossipVotes peerObj PeerChans{..} gossipCh = logOnException $ do
  logger InfoS "Starting routine for gossiping votes" ()
  trace (TePeerGossipVotes TepgvStarted)
  forever $ do
    bchH      <- queryRO blockchainHeight
    peerState <- getPeerState peerObj
    trace (TePeerGossipVotes TepgvNewIter)
    case peerState of
      --
      Lagging p -> do
        trace (TePeerGossipVotes TepgvLagging)
        mcmt <- case lagPeerStep p of
          FullStep peerH _ _
            | peerH == bchH -> queryRO $ retrieveLocalCommit peerH
            | otherwise     -> queryRO $ retrieveCommit      peerH
        --
        case mcmt of
         Just cmt -> do
           let cmtVotes  = Map.fromList [ (signedKeyInfo v, unverifySignature v)
                                        | v <- NE.toList (commitPrecommits cmt) ]
           let peerVotes = Map.fromList
                         $ map (\x -> (ValidatorIdx x,()))
                         $ ISet.toList
                         $ getValidatorIntSet
                         $ lagPeerPrecommits p
               unknown   = Map.difference cmtVotes peerVotes
           case Map.size unknown of
             0 -> return ()
             n -> do i <- liftIO $ randomRIO (0,n-1)
                     let vote = unverifySignature $ toList unknown !! i
                     addPrecommit peerObj vote
                     liftIO $ atomically $ writeTBQueue gossipCh $ GossipPreCommit vote
                     tickSend $ precommit gossipCnts
         Nothing -> return ()
      --
      Current p -> trace (TePeerGossipVotes TepgvCurrent) >> liftIO (atomically consensusState) >>= \case
        Nothing                       -> return ()
        Just (h',_) | h' /= succ bchH -> return ()
        Just (_,tm)                   -> do
          let FullStep _ round _ = peerStep p
              doGosip            = liftIO . atomically . writeTBQueue gossipCh
          let toSet = getValidatorIntSet

          -- Send proposals
          case () of
            _| not $ round `Set.member` peerProposals p
             , Just pr <- round `Map.lookup` smProposals tm
               -> do let prop = signedValue pr
                     addProposal peerObj (propHeight prop) (propRound prop)
                     doGosip $ GossipProposal $ unverifySignature pr
                     tickSend $ proposals gossipCnts
             | otherwise -> return ()
          -- Send prevotes
          case () of
            _| Just localPV <- Map.lookup round $ toPlainMap $ smPrevotesSet tm
             , unknown      <- IMap.difference localPV peerPV
             , not (IMap.null unknown)
               -> do let n = IMap.size unknown
                     i <- liftIO $ randomRIO (0,n-1)
                     let vote = unverifySignature $ toList unknown !! i
                     addPrevote peerObj vote
                     doGosip $ GossipPreVote vote
                     tickSend $ prevote gossipCnts
             | otherwise -> return ()
             where
               peerPV = maybe IMap.empty (IMap.fromSet (const ()) . toSet)
                      $ Map.lookup round
                      $ peerPrevotes p
          -- Send precommits
          case () of
            _| Just localPC <- Map.lookup round $ toPlainMap $ smPrecommitsSet tm
             , unknown      <- IMap.difference localPC peerPC
             , not (IMap.null unknown)
               -> do let n = IMap.size unknown
                     i <- liftIO $ randomRIO (0,n-1)
                     let vote = unverifySignature $ toList unknown !! i
                     addPrecommit peerObj vote
                     doGosip $ GossipPreCommit vote
                     tickSend $ precommit gossipCnts
             | otherwise -> return ()
             where
               peerPC = maybe IMap.empty (IMap.fromSet (const ()) . toSet)
                      $ Map.lookup round
                      $ peerPrecommits p
      Ahead   _ -> trace (TePeerGossipVotes TepgvAhead)   >> return ()
      Unknown   -> trace (TePeerGossipVotes TepgvUnknown) >> return ()
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
                    tickSend $ Logging.tx gossipCnts
      Nothing -> return ()

-- | Gossip blocks with given peer
peerGossipBlocks
  :: (MonadReadDB m alg a, MonadIO m, MonadMask m, MonadLogger m, Serialise a, Crypto alg)
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
                -- FIXME: Partiality
                b <- throwNothing (DBMissingBlock h) <=< queryRO
                   $ retrieveBlock h
                liftIO $ atomically $ writeTBQueue gossipCh $ GossipBlock b
                tickSend $ blocks gossipCnts
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
                   tickSend $ blocks gossipCnts
           --
           | otherwise -> return ()
      -- Nothing to do
      Ahead _ -> return ()
      Unknown -> return ()
    waitSec (0.001 * fromIntegral (gossipDelayBlocks p2pConfig))

-- | Routine for receiving messages from peer
peerReceive
  :: ( MonadReadDB m alg a, MonadIO m, MonadMask m, MonadLogger m
     , Crypto alg, BlockData a)
  => PeerStateObj m alg a
  -> PeerChans m alg a
  -> TChan PexMessage
  -> P2PConnection
  -> MempoolCursor m alg (TX a)
  -> m ()
peerReceive peerSt PeerChans{..} peerExchangeCh P2PConnection{..} MempoolCursor{..} = logOnException $ do
  logger InfoS "Starting routing for receiving messages" ()
  fix $ \loop -> recv >>= \case -- TODO поменять fix на forever, т.к. в последних версиях base они одни и те же
    Nothing  -> logger InfoS "Peer stopping since socket is closed" ()
    Just bs  -> case deserialiseOrFail bs of
      Left  e   -> logger ErrorS ("Deserialization error: " <> showLS e) ()
      Right msg -> do
        -- logger DebugS ("Message received: " <> showGossipMsg msg) ()
        case msg of
          -- Forward to application and record that peer has
          -- given vote/proposal/block
          GossipPreVote   v -> do liftIO $ atomically $ peerChanRx $ RxPreVote v
                                  tickRecv $ prevote gossipCnts
                                  addPrevote peerSt v
          GossipPreCommit v -> do liftIO $ atomically $ peerChanRx $ RxPreCommit v
                                  tickRecv $ precommit gossipCnts
                                  addPrecommit peerSt v
          GossipProposal  p -> do liftIO $ atomically $ peerChanRx $ RxProposal p
                                  tickRecv $ proposals gossipCnts
                                  addProposal peerSt (propHeight (signedValue p))
                                                     (propRound  (signedValue p))
          GossipBlock     b -> do liftIO $ atomically $ peerChanRx $ RxBlock b
                                  tickRecv $ blocks gossipCnts
                                  addBlock peerSt b
          GossipTx tx       -> do void $ pushTransaction tx
                                  tickRecv $ Logging.tx gossipCnts
          GossipPex pexmsg  -> do liftIO $ atomically $ writeTChan peerExchangeCh pexmsg
                                  tickRecv $ pex gossipCnts
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

-- | Routine for actually sending data to peers
peerSend
  :: ( MonadReadDB m alg a, MonadMask m, MonadIO m,  MonadLogger m
     , Crypto alg, BlockData a)
  => NetAddr
  -> PeerStateObj m alg a
  -> PeerChans m alg a
  -> TBQueue (GossipMsg alg a)
  -> P2PConnection
  -> m x
peerSend peerAddrTo peerSt PeerChans{..} gossipCh P2PConnection{..} = logOnException $ do
  logger InfoS ("Starting routing for sending data to " <> showLS peerAddrTo) ()
  ownPeerChanTx  <- liftIO $ atomically $ dupTChan peerChanTx
  ownPeerChanPex <- liftIO $ atomically $ dupTChan peerChanPex
  forever $ do
    msg <- liftIO $ atomically $  fmap GossipAnn (readTChan ownPeerChanTx)
                              <|> readTBQueue gossipCh
                              <|> fmap GossipPex (readTChan ownPeerChanPex)
    -- logger InfoS ("Send to (" <> showLS peerAddrTo <> "): " <> showGossipMsg msg) ()
    send $ serialise msg -- XXX Возможна ли тут гонка? Например, сообщение попало в другую ноду, та уже использует его,
                         --     однако, TCP ACK с той ноды ещё не вернулся на текущую ноду и addBlock/advanceOurHeight
                         --     ещё не успели вызваться.
    -- Update state of peer when we advance to next height
    case msg of
      GossipBlock b                        -> addBlock peerSt b
      GossipAnn (AnnStep (FullStep h _ _)) -> advanceOurHeight peerSt h
      _                                    -> return ()

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
  addrs' <- liftIO $ atomically $ readTChan peerChanPexNewAddresses
  addrs  <- fmap Set.fromList $ filterOutOwnAddresses $ map (`normalizeNodeAddress` Nothing) addrs'
  liftIO $ atomically $ modifyTVar' prKnownAddreses (`Set.union` addrs)


peerPexKnownCapacityMonitor
  :: ( MonadIO m, MonadLogger m)
  => PeerChans m alg a
  -> PeerRegistry
  -> Int
  -> Int
  -> m ()
peerPexKnownCapacityMonitor PeerChans{..} PeerRegistry{..} minKnownConnections _maxKnownConnections = do
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
  :: ( MonadIO m, MonadFork m, MonadMask m, MonadLogger m)
  => PeerChans m alg a
  -> PeerRegistry
  -> TChan PexMessage
  -> TBQueue (GossipMsg alg a)
  -> m ()
peerGossipPeerExchange PeerChans{..} PeerRegistry{prConnected,prIsActive} pexCh gossipCh = forever $
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
        when isSomethingSent $ tickSend $ pex gossipCnts
    connectToAddrs addrs = do
        logger DebugS ("peerGossipPeerExchange: some address received: " <> showLS addrs) ()
        liftIO $ atomically $ writeTChan peerChanPexNewAddresses addrs
    ping = do
        liftIO $ atomically $ writeTBQueue gossipCh (GossipPex PexPong)
        tickSend $ pex gossipCnts
    pong = return ()


