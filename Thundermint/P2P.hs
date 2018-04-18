{-# LANGUAGE DataKinds #-}
-- |
-- Mock P2P
module Thundermint.P2P where

import Control.Monad
import Control.Concurrent.STM
import qualified Data.Map        as Map
import           Data.Map          (Map)
import qualified Data.Set        as Set
import           Data.Set          (Set)

import Thundermint.Crypto
import Thundermint.Consensus.Types
import Thundermint.Blockchain.Types

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Messages which peers exchange with each other
data PeerMsg alg a
  = PMsgHasVote   Height Round VoteType (Address alg)
  -- ^ We have following votes
  | PMsgPreVote   (Signed 'Unverified alg (Vote 'PreVote   alg a))
  | PMsgPreCommit (Signed 'Unverified alg (Vote 'PreCommit alg a))
  | PMsgProposal  (Proposal alg a)
  deriving (Show)

-- | Connection to peer
data Connection alg a = Connection
  { peerConnRx :: TChan (PeerMsg alg a)
  , peerConnTx :: TChan (PeerMsg alg a)
  }

-- | Data structure used to track peer's state.
-- 
-- FIXME: VERY inefficient! Go implementation uses bitmasks
--        to track votes and proposals
data PeerState alg a = PeerState
  { peerHeight     :: Height
    -- ^ Height peer at
  , peerPrevotes   :: Map Round (Set (Address alg))                             
    -- ^ Set of prevotes for current height that peer has
  , peerPrecommits :: Map Round (Set (Address alg))
    -- ^ Set of precommits for current height that peer has
  , peerProposals  :: Set Round
    -- ^ Set of proposals peer has
  }


----------------------------------------------------------------

-- Gossips data from peer
--
--  * Periodically check whether we have data which peer doesn't have
peerGossipRoutine
  :: TVar (PeerState alg a)
  -> IO x
-- FIXME: We need to pass round state here!
--        Should we use TVar (TMState alg a)?
peerGossipRoutine = undefined

-- Receive data from peer
--
--  * Check signature validity if needed. We want to check signatures
--    here since we will want to punish peer sending incorrect messages
--
--  * Forward to application as RxMessage
peerRecvRoutine
  :: AppState alg a         -- We need application state to check signatures.
  -> TVar (PeerState alg a) -- State of peer
  -> IO x
peerRecvRoutine = undefined

-- Send data to peer
peerSendRoutine
  :: TChan (MessageTx alg a) -- Read end for broadcast messages
  -> IO x
peerSendRoutine = undefined

-- -- | Local state. Everything is stored in TVars
-- --
-- --   FIXME: obviously we'll need to persists this data
-- data LocalState alg a = LocalState
--   { locSHeight       :: TVar Height
--   , locSBlockchain   :: TVar (Blockchain alg a)
--   , locSPrevoteSet   :: TVar (HeightVoteSet 'PreVote alg a)
--   , locSPrecommitSet :: TVar (HeightVoteSet 'PreCommit alg a)
--   }
  


-- -- peerSendRoutine ::


-- -- | Here we update state of peer
-- peerRecvRoutine
--   :: TVar  (PeerState alg a)    -- ^ Current state of peer
--   -> TChan (MessageRX alg a)    -- ^ Channel to send messages
--   -> TChan (PeerMsg   alg a)    -- ^ Channel to read messages from
--   -> IO x
-- peerRecvRoutine tvPeer chanRx chanNet = do
--   forever $ do
--     m <- atomically $ readTChan chanNet
--     case m of
--       -- Update peer state
--       --
--       -- FIXME: we need to check that address is valid. Or not???
--       PMsgHasVote h r ty addr -> atomically $ do undefined
--       --
--       PMsgPreVote   vote -> atomically $ writeTChan chanRx $ RxPreVote   vote
--       PMsgPreCommit vote -> atomically $ writeTChan chanRx $ RxPreCommit vote
--       PMsgProposal  p    -> atomically $ writeTChan chanRx $ RxProposal  p


-- peerSendRoutine
--   :: TVar  (PeerState alg a)
--      -- FIXME: do we update state of peer on sending message?
--   -> TChan (MessageTX alg a)
--   -> TChan (PeerMsg   alg a)
--   -> IO x
-- peerSendRoutine tvPeer chanTx chanNet = undefined


-- -- peerNe
-- -- data Bus alg a = Bus
-- --   { busRx :: TChan (MessageRX alg a)
-- --   , busTx :: TChan (MessageTX alg a)
-- --   }

-- -- -- We have 3 thread per peer
-- -- --  1. For sending outgoing messages
-- -- --  2. For receiving data
-- -- --  3. For gossipin

  
-- -- peerGossipRoutine
-- --   :: ()
-- --   => PeerState alg a            -- ^ Initial state of a peer
-- --   -> TChan 





-- -- runPeerExchange :: ???
