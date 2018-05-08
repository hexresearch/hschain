{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Mock P2P
module Thundermint.P2P (
    startPeerDispatcher
  ) where

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Codec.Serialise
import           Data.Foldable
import           Data.Function
import qualified Data.Map        as Map
import           Data.Map          (Map)
import qualified Data.Set        as Set
import           Data.Set          (Set)
import GHC.Generics (Generic)

import Thundermint.Crypto
import Thundermint.Consensus.Types
import Thundermint.Blockchain.Types
import Thundermint.P2P.Network
import Thundermint.Store


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
  }


-- | Connection handed to process controlling communication with peer
data PeerChans addr alg a = PeerChans
  { peerChanTx :: STM (MessageTx alg a)
    -- ^ STM action for getting message to send to peer
  , peerChanRx :: MessageRx 'Unverified alg a -> STM ()
    -- ^ STM action for sending message to main application
  , retreivePeerSet :: STM (Set addr)
    -- ^ Obtain set of all peers
  , sendPeerSet     :: Set addr -> STM ()
    -- ^ Send set of peers to dispatcher
  }


----------------------------------------------------------------
-- Dispatcher
----------------------------------------------------------------

startPeerDispatcher
  :: (Serialise a, Ord addr)
  => NetworkAPI sock addr
  -> AppChans alg a
  -> IO x
startPeerDispatcher net@NetworkAPI{..} AppChans{..} = do
  peers        <- newPeerRegistry
  peerExchange <- newTChanIO
  let peerCh = PeerChans { peerChanTx = readTChan appChanTx
                         , peerChanRx = writeTChan appChanRx
                         , retreivePeerSet = do let PeerRegistry v = peers
                                                m <- readTVar v
                                                return $ Set.fromList $ toList m
                         , sendPeerSet     = writeTChan peerExchange
                         }
  -- Start listening on socket
  registry <- newPeerRegistry
  flip finally (reapPeers registry)
    $ forkLinked (acceptLoop net peerCh registry)
    $ forever $ do
        threadDelay 100000


-- Initiate connection to remote host
connectPeerTo
  :: (Serialise a)
  => NetworkAPI sock addr
  -> addr
  -> PeerChans addr alg a
  -> PeerRegistry addr
  -> IO ()
connectPeerTo net@NetworkAPI{..} addr peerCh registry =
  mask $ \restore -> void $ forkIO $ do
    tid <- myThreadId
    registerPeer registry tid addr
    flip finally (unregisterPeer registry tid) $ do
      sock <- connect addr
      restore (startPeer peerCh (applySocket net sock))
        `finally` close sock

-- Initiate connection to remote host
acceptPeer
  :: (Serialise a)
  => NetworkAPI sock addr
  -> (sock,addr)
  -> PeerChans addr alg a
  -> PeerRegistry addr
  -> IO ()
acceptPeer net@NetworkAPI{..} (sock,addr) peerCh registry =
  mask $ \restore -> void $ forkIO $ do
    tid <- myThreadId
    registerPeer registry tid addr
    flip finally (unregisterPeer registry tid) $ do
      restore (startPeer peerCh (applySocket net sock))
        `finally` close sock

acceptLoop
  :: (Serialise a)
  => NetworkAPI sock addr
  -> PeerChans addr alg a
  -> PeerRegistry addr
  -> IO ()
acceptLoop net@NetworkAPI{..} peerCh registry =
  bracket (listenOn "50000") fst $ \(_,accept) -> forever $ do
    mask $ \restore -> do
      (sock, addr) <- accept
      void $ forkIO $ do
        tid <- myThreadId
        registerPeer registry tid addr
        flip finally (unregisterPeer registry tid) $ do
          restore (startPeer peerCh (applySocket net sock))
            `finally` close sock



newtype PeerRegistry a = PeerRegistry (TVar (Map ThreadId a))

newPeerRegistry :: IO (PeerRegistry a)
newPeerRegistry = PeerRegistry <$> newTVarIO Map.empty

registerPeer :: PeerRegistry a -> ThreadId -> a -> IO ()
registerPeer (PeerRegistry v) tid
  = atomically . modifyTVar' v . Map.insert tid

unregisterPeer :: PeerRegistry a -> ThreadId -> IO ()
unregisterPeer (PeerRegistry v)
  = atomically . modifyTVar' v . Map.delete

reapPeers :: PeerRegistry a -> IO ()
reapPeers (PeerRegistry v)
  = mapM_ killThread . Map.keys =<< readTVarIO v


----------------------------------------------------------------
-- Peer
----------------------------------------------------------------

startPeer
  :: (Serialise a)
  => PeerChans addr alg a  -- ^ Communication with main application
                           --   and peer dispatcher
  -> SendRecv              -- ^ Functions for interaction with network
  -> IO ()
startPeer peerCh@PeerChans{..} net@SendRecv{..} = do
  gossipCh <- newTChanIO
  peerVar  <- newTVarIO PeerState { peerHeight     = Height 0
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
     $ fix $ \loop -> recv 4096 >>= \case
         Nothing -> return ()
         Just bs -> case deserialiseOrFail bs of
           -- FIXME: do something meaningful with decoding error.
           Left  _   -> return ()
           Right msg -> case msg of
             -- Forward to application
             GossipPreVote   v -> atomically $ peerChanRx $ RxPreVote   v
             GossipPreCommit v -> atomically $ peerChanRx $ RxPreCommit v
             GossipProposal  p -> atomically $ peerChanRx $ RxProposal  p
             GossipBlock     b -> atomically $ peerChanRx $ RxBlock     b
             -- Update peer state
             GossipStatus         h _          ->
               atomically $ modifyTVar' peerVar $ \p ->
                 if peerHeight p == h then p
                                      else PeerState { peerHeight     = h
                                                     , peerPrevotes   = Map.empty
                                                     , peerPrecommits = Map.empty
                                                     , peerProposals  = Set.empty
                                                     , peerBlocks     = Set.empty
                                                     }

             GossipHasProposals   h props      ->
               atomically $ modifyTVar' peerVar $ \p ->
                 if peerHeight p == h then p
                                      else p { peerProposals = props }
             GossipHasPrevotes    h prevotes   ->
               atomically $ modifyTVar' peerVar $ \p ->
                 if peerHeight p == h then p
                                      else p { peerPrevotes = prevotes }
             GossipHasPrecommits  h precommtis ->
               atomically $ modifyTVar' peerVar $ \p ->
                 if peerHeight p == h then p
                                      else p { peerPrecommits = precommtis }
             GossipHasPropBlocks  h bids       ->
               atomically $ modifyTVar' peerVar $ \p ->
                 if peerHeight p == h then p
                                      else p { peerBlocks = bids }
             --




-- | Gossip blocks to peer
peerGossipBlocks
  :: ()
  => PeerChans addr alg a
  -> TChan (GossipMsg alg a)
  -> TVar (PeerState alg a)
  -> IO x
peerGossipBlocks PeerChans{..} chan peerVar = forever $ do
  -- 1. Check that we have more blocks that peer
  -- 2. Send random block that we have and peer don't
  -- 3. Sleep
  undefined
  threadDelay 100e3

-- | Gossip votes with given peer
peerGossipVotes
  :: ()
  => PeerChans addr alg a
  -> TChan (GossipMsg alg a)
  -> TVar (PeerState alg a)
  -> IO x
peerGossipVotes PeerChans{..} chan peerVar = forever $ do
  -- 1. Check that we have more votes that peer
  -- 2. Send random block that we have and peer don't
  -- 3. Sleep
  undefined
  threadDelay 100e3


peerSendGossip
  :: (Serialise a)
  => TChan (GossipMsg alg a)
  -> STM (MessageTx alg a)
  -> SendRecv
  -> IO x
peerSendGossip gossipCh readTx SendRecv{..} = forever $ do
  msg <- atomically $ fromApp <|> readTChan gossipCh
  send $ serialise msg
  where
    fromApp = readTx >>= return . \case
      TxPreVote   v -> GossipPreVote   $ unverifySignature v
      TxPreCommit v -> GossipPreCommit $ unverifySignature v
      TxProposal  p -> GossipProposal  $ unverifySignature p


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Fork thread. Any exception except `AsyncException` in forked
--   thread is forwarded to original thread.
forkLinked :: IO a              -- ^ Action to execute in forked thread
           -> IO b              -- ^ What to do while thread executes
           -> IO b
forkLinked action io = do
  tid <- myThreadId
  let fini (Right _) = return ()
      fini (Left  e) = case fromException e of
        Just (_ :: AsyncException) -> return ()
        _                          -> throwTo tid e
  bracket (forkFinally action fini)
          killThread
          (const io)
