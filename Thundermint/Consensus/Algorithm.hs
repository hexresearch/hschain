{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns     #-}
-- |
-- Implementation of tendermint consensus protocol. Note that this
-- module provides only implementation of consensus state machine and
-- assumes that everything sent to it is already validated! In
-- particular dealing with block content is delegated to other parts
-- of library
module Thundermint.Consensus.Algorithm where

import Control.Monad
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Map   (Map)
import qualified Data.Map as Map

import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Consensus.Types

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Step of the algorithm
data Step
  = StepProposal
  | StepPrevote
  | StepPrecommit
  deriving (Show,Eq,Ord)


-- | State for tendermint consensus at some particular height.
data TMState alg a = TMState
  { smRound            :: Round
    -- ^ Current round
  , smStep             :: Step
    -- ^ Current step in the round
  , smPrevotesSet      :: HeightVoteSet 'PreVote alg a
    -- ^ Set of all received valid prevotes
  , smPrecommitsSet    :: HeightVoteSet 'PreCommit alg a
    -- ^ Set of all received valid precommits
  , smProposal         :: Maybe (Proposal alg a)
    -- ^ Proposal for current round
  , smLockedBlock      :: Maybe (Round, Block alg a, BlockID alg a)
    -- ^ Round and block we're locked on
  , smLastCommit       :: Maybe (Commit alg a)
    -- ^ Commit for previous block. Nothing if previous block is
    --   genesis block.
  }

data Validator alg = Validator
  { validatorPubKey      :: PublicKey alg
  , validatorVotingPower :: Int64
  }

data PrivValidator alg a = PrivValidator
  { validatorPrivKey  :: PrivKey alg
  , validateBlockData :: a -> Bool
  }

data HeightParameres alg a = HeightParameres
  { validatorSet  :: Map (Address alg) (Validator alg)
  , privValidator :: PrivValidator alg a
  , currentH      :: Height
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
  -- | Schedule timeout
  scheduleTimeout :: Timeout -> m ()

  -- | Create new proposal
  makeProposal   :: m (Proposal alg a)
  areWeProposers :: Round -> m Bool

  -- | Validate proposal
  isProposalValid :: Proposal alg a -> m Bool

  -- FIXME: types? Constrain alg???
  castPrevote   :: (Time -> Vote 'PreVote   alg a) -> m ()
  castPrecommit :: (Time -> Vote 'PreCommit alg a) -> m ()
  commitBlock   :: BlockID alg a -> TMState alg a -> m x

  proposeBlock  :: Proposal alg a -> m ()

data Message alg a
  = ProposalMsg  (Proposal alg a)
  | PreVoteMsg   (Signed 'Verified alg (Vote 'PreVote   alg a))
  | PreCommitMsg (Signed 'Verified alg (Vote 'PreCommit alg a))
  | TimeoutMsg   Timeout
  deriving Show

data Timeout = Timeout Height Round Step
  deriving (Show,Eq,Ord)

-- | Transition rule for tendermint state machine. State is passed
--   explicitly and we track effects like sending message and
--   committing in the monad.
tendermintTransition
  :: (Crypto alg, ConsensusMonad m)
  => HeightParameres alg a  -- ^ Parameters for current height
  -> Message alg a          -- ^ Message which causes state transition
  -> TMState alg a          -- ^ Initial state of state machine
  -> m (TMState alg a)
tendermintTransition par@HeightParameres{..} msg sm@TMState{..} =
  case msg of
    -- Receiving proposal by itself does not entail state transition.
    -- We leave PROPOSE only after timeout
    ProposalMsg p@Proposal{..}
      -- We already have proposal ignore this
      --
      -- FIXME: should we track double proposals?
      --        tendermint implementation have same question
      | Just _ <- smProposal   -> tranquility
      -- Proposal is from wrong height/round. Ignore it.
      --
      -- FIXME: should we store signed proposal for later rounds?
      | propRound  /= smRound  -> tranquility
      | propHeight /= currentH -> tranquility
      -- Accept proposal
      --
      -- FIXME: validate proposal.
      | otherwise             -> return sm { smProposal = Just p }
    -- ----------------------------------------------------------------
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
  => HeightParameres alg a
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


-- Check whether we need to perform any state transition after we
-- received precommit
checkTransitionPrecommit
  :: (Crypto alg, ConsensusMonad m)
  => HeightParameres alg a
  -> Round
  -> TMState alg a
  -> m (TMState alg a)
checkTransitionPrecommit par r sm@(TMState{..})
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
    = commitBlock bid sm
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
  => HeightParameres alg a
  -> Round
  -> TMState alg a
  -> m (TMState alg a)
enterPropose par@HeightParameres{..} r sm@TMState{..} = do
  -- We are proposer. Broadcast proposal otherwise do nothing
  scheduleTimeout $ Timeout currentH r StepProposal
  makeP <- areWeProposers smRound
  when makeP $ case smLockedBlock of
    -- FIXME: take care of POL fields of proposal
    -- If we're locked on block we MUST propose it
    Just (lockR,bid,_) -> do proposeBlock undefined
    -- Otherwise we need to create new block from mempool
    Nothing          -> do p <- makeProposal
                           proposeBlock undefined
  undefined -- FIXME: fix logic for proposals

-- Enter PREVOTE step. Upon entering it we:
--
--   1. Select and broadcast prevote
--   2. Call checkTransitionPrevote to check whether we need to go to
--      PRECOMMIT
enterPrevote
  :: (ConsensusMonad m, Crypto alg)
  => HeightParameres alg a
  -> Round
  -> TMState alg a
  -> m (TMState alg a)
enterPrevote par@HeightParameres{..} r (unlockOnPrevote -> sm@TMState{..}) = do
  castPrevote $ \t -> Vote { voteHeight  = currentH
                           , voteRound   = smRound
                           , voteTime    = t
                           , voteBlockID = prevoteBlock
                           }
  scheduleTimeout $ Timeout currentH r StepPrevote
  checkTransitionPrevote par smRound sm { smStep = StepPrevote }
  where
    prevoteBlock
      -- We're locked on block so we prevote it
      | Just (_,_,bid) <- smLockedBlock = Just bid
      -- FIXME: when we check that block is good???
      --
      -- Proposal from current round is good. Prevote it
      | Just p <- smProposal            = Just (propBlockID p)
      -- Proposal invalid or absent. Prevote NIL
      | otherwise                       = Nothing

-- Unlock upon entering prevote which happens if:
--   * We're already locked
--   * We have polca for round R: lock.R < R < current.R
unlockOnPrevote
  :: (Crypto alg)
  => TMState alg a
  -> TMState alg a
unlockOnPrevote sm@TMState{..}
  | Just (lockR, _, _) <- smLockedBlock
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
  => HeightParameres alg a
  -> Round
  -> TMState alg a
  -> m (TMState alg a)
enterPrecommit par@HeightParameres{..} r sm@TMState{..} = do
  castPrecommit $ \t -> Vote { voteHeight  = currentH
                             , voteRound   = smRound
                             , voteTime    = t
                             , voteBlockID = precommitBlock
                             }
  scheduleTimeout $ Timeout currentH r StepPrecommit
  checkTransitionPrecommit par r sm
    { smStep        = StepPrecommit
    , smRound       = r
    , smLockedBlock = lock
    }
  where
    (precommitBlock,lock)
      -- We have polka at round R
      | Just v <- majority23at r smPrecommitsSet
        = case voteBlockID v of
            -- Polca for NIL. Unlock and precommit NIL
            Nothing  -> ( Nothing
                        , Nothing)
            -- Polca for block B. Re-lock to B and precommit it
            Just bid -> ( Just bid
                        , Just (r,undefined,bid))
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
