{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns     #-}
-- |
-- Implementation of tendermint consensus protocol.
--
-- Note that this
-- module provides only implementation of consensus state machine and
-- assumes that everything sent to it is already validated! In
-- particular dealing with block content is delegated to other parts
-- of library
module Thundermint.Consensus.Algorithm (
    -- * Data types
    ProposalState(..)
  , Message(..)
  , TMState(..)
  , HeightParameres(..)
    -- * State transitions
  , ConsensusMonad(..)
  , newHeight
  , tendermintTransition
  ) where

import Control.Monad
import qualified Data.Map        as Map
import           Data.Map          (Map)

import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Consensus.Types


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Messages being sent to consensus engine
data Message alg a
  = ProposalMsg    (Proposal alg a)
    -- ^ Incoming proposal
  | PreVoteMsg     (Signed 'Verified alg (Vote 'PreVote   alg a))
    -- ^ Incoming prevote
  | PreCommitMsg   (Signed 'Verified alg (Vote 'PreCommit alg a))
    -- ^ Incoming precommit
  | TimeoutMsg     Timeout
    -- ^ Timeout
  deriving Show

-- | Set of parameters for consensus algorithm for given height. These
--   parameters are constant for the duration of data
data HeightParameres (m :: * -> *) alg a = HeightParameres
  { currentH            :: Height
    -- ^ Height we're on.
  , areWeProposers      :: Round -> Bool
    -- ^ Find address of proposer for given round.
  , validateBlock       :: BlockID alg a -> m ProposalState
    -- ^ Request validation of particular block

  , scheduleTimeout     :: Timeout -> m ()
    -- ^ Schedule timeout.
  , broadcastProposal   :: Round -> BlockID alg a -> m ()
    -- ^ Broadcast proposal for given round and block.
  , castPrevote         :: Round -> Maybe (BlockID alg a) -> m ()
    -- ^ Broadcast prevote for particular block ID in some round.
  , castPrecommit       :: Round -> Maybe (BlockID alg a) -> m ()
    -- ^ Broadcast precommit for particular block ID in some round.
  , createProposal      :: Round -> Maybe (Commit alg a) -> m (BlockID alg a)
    -- ^ Create new proposal block. Block itself should be stored
    --   elsewhere.
  , commitBlock         :: forall x. Commit alg a -> m x
    -- ^ We're done for this height. Commit block to blockchain

  , logger              :: String -> m ()
  }



----------------------------------------------------------------
-- Consensus
----------------------------------------------------------------

-- | Type class for monads encapsulating effects which could occur
--   when evaluating state transition for tendermint algorithm
class Monad m => ConsensusMonad m where
  -- | No change to state should happen. Abort execution. Note this is
  --   not an error
  tranquility :: m a
  -- | We detected clearly malicious behavior from peer
  --
  --   FIXME: store malicious behavior as evidence.
  misdeed :: m a
  -- | Something went horribly wrong. Our implementation is buggy only
  --   thing we can do is to die with honor
  panic :: String -> m a

newHeight
  :: (Crypto alg, ConsensusMonad m)
  => HeightParameres m alg a
  -> TMState alg a
  -> m (TMState alg a)
newHeight par = enterPropose par (Round 0)

-- | Transition rule for tendermint state machine. State is passed
--   explicitly and we track effects like sending message and
--   committing in the monad.
--
--   Note that when state machine sends vote or proposal it does not
--   update state and thus message should be send back to it
tendermintTransition
  :: (Crypto alg, ConsensusMonad m)
  => HeightParameres m alg a  -- ^ Parameters for current height
  -> Message alg a          -- ^ Message which causes state transition
  -> TMState alg a          -- ^ Initial state of state machine
  -> m (TMState alg a)
tendermintTransition par@HeightParameres{..} msg sm@TMState{..} =
  case msg of
    -- Receiving proposal by itself does not entail state transition.
    -- We leave PROPOSE only after timeout
    ProposalMsg p@Proposal{..}
      -- Ignore proposal from wrong height
      | propHeight /= currentH
        -> tranquility
      -- We have proposal already
      --
      -- FIXME: should we track double proposals? Tendermint
      --        implementation have same question
      | Just _ <- Map.lookup propRound smProposals
        -> tranquility
      -- Add it to map of proposals
      | otherwise -> return sm { smProposals = Map.insert propRound p smProposals }
    ----------------------------------------------------------------
    PreVoteMsg v@(signedValue -> Vote{..})
      -- Only accept votes with current height
      | voteHeight /= currentH -> tranquility
      | otherwise              -> checkTransitionPrevote par voteRound
                              =<< addPrevote v sm
    ----------------------------------------------------------------
    PreCommitMsg v@(signedValue -> Vote{..})
      -- FIXME: store precommits from previous height if they validate commit
      --
      -- Only accept votes with current height
      | voteHeight /= currentH -> tranquility
      | otherwise              -> checkTransitionPrecommit par voteRound
                              =<< addPrecommit v sm
    ----------------------------------------------------------------
    TimeoutMsg t ->
      case compare t t0 of
        -- It's timeout from previous steps. Ignore it
        LT -> tranquility
        -- Timeout from future. Must not happen
        GT -> panic $ "Timeout from future: " ++ show (t,t0)
        -- Update state accordingly. We unconditionally enter next step of
        -- round or next round.
        --
        -- FIXME: specification is unclear about this point but go
        --        implementation advances unconditionally
        EQ -> case smStep of
          StepProposal  -> enterPrevote   par smRound        sm
          StepPrevote   -> enterPrecommit par smRound        sm
          StepPrecommit -> enterPropose   par (next smRound) sm
      where
        t0 = Timeout currentH smRound smStep


-- Check whether we need to perform any state transition after we
-- received prevote
checkTransitionPrevote
  :: (Crypto alg, ConsensusMonad m)
  => HeightParameres m alg a
  -> Round
  -> TMState alg a
  -> m (TMState alg a)
checkTransitionPrevote par@HeightParameres{..} r sm@(TMState{..})
  --  * We have +2/3 prevotes for some later round (R+x)
  --  => goto Prevote(H,R+x)
  | r > smRound
  , any23at r smPrevotesSet
    = enterPrevote par r sm
  --  * We have +2/3 prevotes for some block/nil in current round
  --  * We are in prevote step
  --  => goto Precommit(H,R)
  | r      == smRound
  , smStep == StepPrevote
  , Just _ <- majority23at r smPrevotesSet
    = enterPrecommit par r sm
  | otherwise
    = return sm

-- Check whether we need to perform any state transition after we
-- received precommit
checkTransitionPrecommit
  :: (Crypto alg, ConsensusMonad m)
  => HeightParameres m alg a
  -> Round
  -> TMState alg a
  -> m (TMState alg a)
checkTransitionPrecommit par@HeightParameres{..} r sm@(TMState{..})
  --  * We have +2/3 precommits for particular block at some round
  --  => goto Commit(H,R)
  --
  --  NOTE: accepting precommits from any round is necessary to ensure
  --        liveness. Consider four honest validators A, B, C, D. Let
  --        all of them precommit block X at round R. A and B do not
  --        receive precommits from C & D in time and move to the next
  --        round while C and D receive all messages and move to next
  --        height. A & B will never receive +2/3 vote at rounds R+x
  --        since there are only 2 validators at height H. But at some
  --        later moment they'll get
  | Just Vote{..} <- majority23at r smPrecommitsSet
  , Just bid      <- voteBlockID
    = do logger $ "COMMMITING: " ++ show bid
         commitBlock Commit{ commitBlockID    = bid
                           , commitPrecommits = valuesAtR r smPrecommitsSet
                           }
  --  * We have +2/3 precommits for nil at current round
  --  * We are at Precommit step [FIXME?]
  --  => goto Propose(H,R+1)
  | r == smRound
  , Just Vote{..} <- majority23at r smPrecommitsSet
  , Nothing       <- voteBlockID
    = enterPropose par (next r) sm
  --  * We have +2/3 precommits for some round (R+x)
  --  => goto Precommit(H,R+x)
  | r > smRound
  , any23at r smPrecommitsSet
    = enterPrecommit par r sm
  -- No change
  | otherwise
    = return sm

-- Enter Propose stage and send required messages
enterPropose
  :: (Crypto alg, ConsensusMonad m)
  => HeightParameres m alg a
  -> Round
  -> TMState alg a
  -> m (TMState alg a)
enterPropose HeightParameres{..} r sm@TMState{..} = do
  logger "ENTER PROPOSE"
  scheduleTimeout $ Timeout currentH r StepProposal
  -- If we're proposers we need to broadcast proposal. Otherwise we do
  -- nothing
  when (areWeProposers r) $ case smLockedBlock of
    -- FIXME: take care of POL fields of proposal
    --
    -- If we're locked on block we MUST propose it
    Just (_,bid) -> do broadcastProposal r bid
    -- Otherwise we need to create new block from mempool
    Nothing      -> do bid <- createProposal r smLastCommit
                       broadcastProposal r bid
  return sm { smRound = r
            , smStep  = StepProposal
            }

-- Enter PREVOTE step. Upon entering it we:
--
--   1. Select and broadcast prevote
--   2. Call checkTransitionPrevote to check whether we need to go to
--      PRECOMMIT
enterPrevote
  :: (ConsensusMonad m, Crypto alg)
  => HeightParameres m alg a
  -> Round
  -> TMState alg a
  -> m (TMState alg a)
enterPrevote par@HeightParameres{..} r (unlockOnPrevote -> sm@TMState{..}) = do
  --
  logger "ENTER PREVOTE"
  castPrevote smRound =<< prevoteBlock
  --
  scheduleTimeout $ Timeout currentH r StepPrevote
  checkTransitionPrevote par r sm
    { smRound = r
    , smStep  = StepPrevote
    }
  where
    prevoteBlock
      -- We're locked on block so we prevote it
      | Just (_,bid) <- smLockedBlock      = return (Just bid)
      -- We have proposal. Prevote it if it's good
      | Just p <- Map.lookup r smProposals =
          validateBlock (propBlockID p) >>= \case
            GoodProposal    -> return (Just (propBlockID p))
            InvalidProposal -> return Nothing
            -- FIXME: tendermint allows prevote for blocks not yet
            --        seen but let be conservative
            UnseenProposal  -> return Nothing
      -- Proposal invalid or absent. Prevote NIL
      | otherwise                          = return Nothing

-- Unlock upon entering prevote which happens if:
--   * We're already locked
--   * We have polca for round R: lock.R < R < current.R
unlockOnPrevote
  :: (Crypto alg)
  => TMState alg a
  -> TMState alg a
unlockOnPrevote sm@TMState{..}
  | Just (lockR, _) <- smLockedBlock
  , hasAnyPolca lockR
    = sm { smLockedBlock = Nothing }
  | otherwise
    = sm
  where
    hasAnyPolca lockR  = not $ null
      [ ()
      | r      <- rangeExclusive lockR smRound
      , Just _ <- [majority23at r smPrevotesSet]
      ]

-- Enter PRECOMMIT stage.
--   1. Select block to precommits and change lock state if necessary
--   2. Broadcast precommit vote
--   3. Check whether we need to make further state transitions.
enterPrecommit
  :: (ConsensusMonad m, Crypto alg)
  => HeightParameres m alg a
  -> Round
  -> TMState alg a
  -> m (TMState alg a)
enterPrecommit par@HeightParameres{..} r sm@TMState{..} = do
  logger "ENTER PRECOMMIT"
  castPrecommit r precommitBlock
  scheduleTimeout $ Timeout currentH r StepPrecommit
  checkTransitionPrecommit par r sm
    { smStep        = StepPrecommit
    , smRound       = r
    , smLockedBlock = lock
    }
  where
    (precommitBlock,lock)
      -- We have polka at round R
      | Just v <- majority23at r smPrevotesSet
        = case voteBlockID v of
            -- Polca for NIL. Unlock and precommit NIL
            Nothing  -> ( Nothing
                        , Nothing)
            -- Polca for block B. Re-lock to B and precommit it
            Just bid -> ( Just bid
                        , Just (r,bid))
       -- Otherwise keep lock unchanged and precommit NIL
      | otherwise
        = (Nothing, smLockedBlock)


addPrevote
  :: (ConsensusMonad m, Crypto alg)
  => Signed 'Verified alg (Vote 'PreVote alg a)
  -> TMState alg a
  -> m (TMState alg a)
addPrevote v sm@TMState{..} =
  case addSignedValue (voteRound $ signedValue v) v smPrevotesSet of
    InsertOK votes   -> return sm{ smPrevotesSet = votes }
    InsertDup        -> tranquility
    InsertConflict _ -> misdeed

addPrecommit
  :: (ConsensusMonad m, Crypto alg)
  => Signed 'Verified alg (Vote 'PreCommit alg a)
  -> TMState alg a
  -> m (TMState alg a)
addPrecommit v sm@TMState{..} =
  case addSignedValue (voteRound $ signedValue v) v smPrecommitsSet of
    InsertOK votes   -> return sm{ smPrecommitsSet = votes }
    InsertDup        -> tranquility
    InsertConflict _ -> misdeed
