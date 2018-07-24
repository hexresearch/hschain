{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
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
    -- * Data types used for logging
  , LogTransitionReason(..)
  , LogTransition(..)
  , LogProposal(..)
  , LogCommit(..)
  ) where

import Control.Monad

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.TH       as JSON
import           Data.Monoid           ((<>))
import qualified Data.Map            as Map
import qualified Data.HashMap.Strict as HM
import           Katip (Severity(..))
import qualified Katip
import GHC.Generics

import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Consensus.Types
import Thundermint.Logger



----------------------------------------------------------------
--
----------------------------------------------------------------



-- | Messages being sent to consensus engine
data Message alg a
  = ProposalMsg    (Signed 'Verified alg (Proposal alg a))
    -- ^ Incoming proposal
  | PreVoteMsg     (Signed 'Verified alg (Vote 'PreVote   alg a))
    -- ^ Incoming prevote
  | PreCommitMsg   (Signed 'Verified alg (Vote 'PreCommit alg a))
    -- ^ Incoming precommit
  | TimeoutMsg     Timeout
    -- ^ Timeout
  deriving Show

-- | Set of parameters and callbacks for consensus algorithm for given
--   height. These parameters are constant while we're deciding on
--   next block.
data HeightParameres (m :: * -> *) alg a = HeightParameres
  { currentH            :: Height
    -- ^ Height we're on.
  , areWeProposers       :: Round -> Bool
    -- ^ Find address of proposer for given round.
  , proposerForRound     :: Round -> Address alg
    -- ^ Proposer for given round
  , validateBlock        :: BlockID alg a -> m ProposalState
    -- ^ Request validation of particular block

  , scheduleTimeout      :: Timeout -> m ()
    -- ^ Schedule timeout. It's called whenever we enter new step so
    --   it could be overloaded to announce to peers change of state
  , broadcastProposal    :: Round -> BlockID alg a -> Maybe (Round, BlockID alg a) -> m ()
    -- ^ Broadcast proposal for given round and block.
  , castPrevote          :: Round -> Maybe (BlockID alg a) -> m ()
    -- ^ Broadcast prevote for particular block ID in some round.
  , castPrecommit        :: Round -> Maybe (BlockID alg a) -> m ()
    -- ^ Broadcast precommit for particular block ID in some round.

  , acceptBlock          :: Round -> BlockID alg a -> m ()
    -- ^ Callback to signal that given block from given round should
    --   be accepted
  , announceHasPreVote   :: Signed 'Verified alg (Vote 'PreVote alg a)   -> m ()
    -- ^ Broadcast to peers announcement that we have given prevote
  , announceHasPreCommit :: Signed 'Verified alg (Vote 'PreCommit alg a) -> m ()
    -- ^ Broadcast to peers announcement that we have given precommit
  , announceStep         :: FullStep -> m ()

  , createProposal       :: Round -> Maybe (Commit alg a) -> m (BlockID alg a)
    -- ^ Create new proposal block. Block itself should be stored
    --   elsewhere.
  , commitBlock          :: forall x. Commit alg a -> m x
    -- ^ We're done for this height. Commit block to blockchain
  }


----------------------------------------------------------------
-- Logging types
----------------------------------------------------------------

data LogTransitionReason
  = Reason'Timeout
  -- ^ Transition is due to timeout
  | Reason'PV_LaterR
  -- ^ We have seen +2/3 prevotes for some later round
  | Reason'PV_Maj
  -- ^ We have seen +2/3 majority of prevotes for some block\/NIL in
  --   current round
  | Reason'PC_Nil
  -- ^ We have +2/3 precommits for NIL
  | Reason'PC_LaterR
  -- ^ We have seen +2/3 precommits for some later round
  deriving (Show,Generic)

JSON.deriveJSON JSON.defaultOptions
  { JSON.constructorTagModifier = drop 7 } ''LogTransitionReason

-- | Description of state transition of
data LogTransition = LogTransition
  { transition'H      :: Height
  , transition'R      :: Round
  , transition'S      :: Step
  , transition'newR   :: Round
  , transition'reason :: LogTransitionReason
  }
JSON.deriveJSON JSON.defaultOptions
  { JSON.fieldLabelModifier = drop 11 } ''LogTransition

instance Katip.ToObject LogTransition
instance Katip.LogItem  LogTransition where
  payloadKeys Katip.V0 _ = Katip.SomeKeys ["H","R","S","newR"]
  payloadKeys _        _ = Katip.AllKeys

-- | Description of proposal
data LogProposal alg a = LogProposal
  { proposal'H   :: Height
  , proposal'R   :: Round
  , proposal'bid :: BlockID alg a
  }

instance Katip.ToObject (LogProposal alg a) where
  toObject p = HM.fromList [ ("H",   JSON.toJSON (proposal'H p))
                           , ("R",   JSON.toJSON (proposal'R p))
                           , ("bid", JSON.toJSON $ let BlockHash _ hash _ = proposal'bid p
                                                   in hash
                             )]
instance Katip.LogItem (LogProposal alg a) where
  payloadKeys Katip.V0 _ = Katip.SomeKeys ["H","R"]
  payloadKeys _        _ = Katip.AllKeys

-- | Description of commit
data LogCommit alg a = LogCommit
  { commit'H   :: Height
  , commit'bid :: BlockID alg a
  }
instance Katip.ToObject (LogCommit alg a) where
  toObject p = HM.fromList [ ("H",   JSON.toJSON (commit'H p))
                           , ("bid", JSON.toJSON $ let BlockHash _ hash _ = commit'bid p
                                                   in hash
                             )]
instance Katip.LogItem (LogCommit alg a) where
  payloadKeys _ _ = Katip.AllKeys

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

-- | Enter new height and create new state for state machine
newHeight
  :: (ConsensusMonad m, MonadLogger m)
  => HeightParameres m alg a
  -> Maybe (Commit alg a)
  -> ValidatorSet alg
  -> m (TMState alg a)
newHeight HeightParameres{..} lastCommit vset = do
  logger InfoS "Entering new height ----------------" currentH
  scheduleTimeout $ Timeout  currentH (Round 0) StepNewHeight
  announceStep    $ FullStep currentH (Round 0) StepNewHeight
  return TMState
    { smRound         = Round 0
    , smStep          = StepNewHeight
    , smPrevotesSet   = emptySignedSetMap vset
    , smPrecommitsSet = emptySignedSetMap vset
    , smProposals     = Map.empty
    , smLockedBlock   = Nothing
    , smLastCommit    = lastCommit
    }


-- | Transition rule for tendermint state machine. State is passed
--   explicitly and we track effects like sending message and
--   committing in the monad.
--
--   Note that when state machine sends vote or proposal it does not
--   update state and thus message should be send back to it
tendermintTransition
  :: (Crypto alg, ConsensusMonad m, MonadLogger m)
  => HeightParameres m alg a  -- ^ Parameters for current height
  -> Message alg a          -- ^ Message which causes state transition
  -> TMState alg a          -- ^ Initial state of state machine
  -> m (TMState alg a)
tendermintTransition par@HeightParameres{..} msg sm@TMState{..} =
  case msg of
    -- Receiving proposal by itself does not entail state transition.
    -- We leave PROPOSE only after timeout
    ProposalMsg p@(signedValue -> Proposal{..})
      -- Ignore proposal from wrong height
      | propHeight /= currentH
        -> tranquility
      -- We have proposal already
      --
      -- FIXME: should we track double proposals? Tendermint
      --        implementation have same question
      | Just _ <- Map.lookup propRound smProposals
        -> tranquility
      -- Node sending message out of order is clearly byzantine
      | signedAddr p /= proposerForRound propRound
        -> misdeed
      -- Add it to map of proposals
      | otherwise
        -> do logger InfoS "Got proposal" $ LogProposal currentH smRound propBlockID
              acceptBlock propRound propBlockID
              return sm { smProposals = Map.insert propRound p smProposals }
    ----------------------------------------------------------------
    PreVoteMsg v@(signedValue -> Vote{..})
      -- Only accept votes with current height
      | voteHeight /= currentH -> tranquility
      | otherwise              -> checkTransitionPrevote par voteRound
                              =<< addPrevote par v sm
    ----------------------------------------------------------------
    PreCommitMsg v@(signedValue -> Vote{..})
      -- Collect stragglers precommits for inclusion of
      | next voteHeight == currentH
      , smStep == StepNewHeight
      , Just cmt <- smLastCommit
        -> case Just (commitBlockID cmt) /= voteBlockID of
             False -> misdeed
             True  -> return sm
               { smLastCommit = Just cmt { commitPrecommits = v : commitPrecommits cmt } }
      -- Only accept votes with current height
      | voteHeight /= currentH -> tranquility
      | otherwise              -> checkTransitionPrecommit par voteRound
                              =<< addPrecommit par v sm
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
        EQ -> do
          case smStep of
            StepNewHeight -> enterPropose   par smRound        sm Reason'Timeout
            StepProposal  -> enterPrevote   par smRound        sm Reason'Timeout
            StepPrevote   -> enterPrecommit par smRound        sm Reason'Timeout
            StepPrecommit -> enterPropose   par (next smRound) sm Reason'Timeout
      where
        t0 = Timeout currentH smRound smStep


-- Check whether we need to perform any state transition after we
-- received prevote
checkTransitionPrevote
  :: (ConsensusMonad m, MonadLogger m, Crypto alg)
  => HeightParameres m alg a
  -> Round
  -> TMState alg a
  -> m (TMState alg a)
checkTransitionPrevote par@HeightParameres{..} r sm@(TMState{..})
  --  * We have +2/3 prevotes for some later round (R+x)
  --  => goto Prevote(H,R+x)
  | r > smRound
  , any23at r smPrevotesSet
    = enterPrevote par r sm Reason'PV_LaterR
  --  * We have +2/3 prevotes for some block/nil in current round
  --  * We are in prevote step
  --  => goto Precommit(H,R)
  | r      == smRound
  , smStep == StepPrevote
  , Just _ <- majority23at r smPrevotesSet
    = enterPrecommit par r sm Reason'PV_Maj
  | otherwise
    = return sm

-- Check whether we need to perform any state transition after we
-- received precommit
checkTransitionPrecommit
  :: (ConsensusMonad m, MonadLogger m, Crypto alg)
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
    = do logger InfoS "Decision to commit" $ LogCommit currentH bid
         acceptBlock voteRound bid
         commitBlock Commit{ commitBlockID    = bid
                           , commitPrecommits = valuesAtR r smPrecommitsSet
                           }
  --  * We have +2/3 precommits for nil at current round
  --  * We are at Precommit step [FIXME?]
  --  => goto Propose(H,R+1)
  | r == smRound
  , Just Vote{..} <- majority23at r smPrecommitsSet
  , Nothing       <- voteBlockID
    = enterPropose par (next r) sm Reason'PC_Nil
  --  * We have +2/3 precommits for some round (R+x)
  --  => goto Precommit(H,R+x)
  | r > smRound
  , any23at r smPrecommitsSet
    = enterPrecommit par r sm Reason'PV_LaterR
  -- No change
  | otherwise
    = return sm

-- Enter Propose stage and send required messages
enterPropose
  :: (ConsensusMonad m, MonadLogger m)
  => HeightParameres m alg a
  -> Round
  -> TMState alg a
  -> LogTransitionReason
  -> m (TMState alg a)
enterPropose HeightParameres{..} r sm@TMState{..} reason = do
  logger InfoS "Entering propose" $ LogTransition currentH smRound smStep r reason
  scheduleTimeout $ Timeout currentH r StepProposal
  announceStep    $ FullStep currentH r StepProposal
  -- If we're proposers we need to broadcast proposal. Otherwise we do
  -- nothing
  when (areWeProposers r) $ case smLockedBlock of
    -- FIXME: take care of POL fields of proposal
    --
    -- If we're locked on block we MUST propose it
    Just (br,bid) -> do logger InfoS "Making POL proposal" $ LogProposal currentH smRound bid
                        broadcastProposal r bid (Just (br,bid))
    -- Otherwise we need to create new block from mempool
    Nothing      -> do bid <- createProposal r smLastCommit
                       logger InfoS "Making new proposal" $ LogProposal currentH smRound bid
                       broadcastProposal r bid Nothing
  return sm { smRound = r
            , smStep  = StepProposal
            }

-- Enter PREVOTE step. Upon entering it we:
--
--   1. Select and broadcast prevote
--   2. Call checkTransitionPrevote to check whether we need to go to
--      PRECOMMIT
enterPrevote
  :: (Crypto alg, ConsensusMonad m, MonadLogger m)
  => HeightParameres m alg a
  -> Round
  -> TMState alg a
  -> LogTransitionReason
  -> m (TMState alg a)
enterPrevote par@HeightParameres{..} r (unlockOnPrevote -> sm@TMState{..}) reason = do
  --
  logger InfoS "Entering prevote" $ LogTransition currentH smRound smStep r reason
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
      | Just (signedValue -> Proposal{..}) <- Map.lookup r smProposals =
          -- If proposal have proof of lock we must have proof
          -- of lock for same round and some block
          case propPOL of
            Just (lockR, lockB) ->
              case majority23at lockR smPrevotesSet of
                Just v | voteBlockID v == Just lockB -> checkPrevoteBlock propBlockID
                       | otherwise                   -> return Nothing --  FIXME: Byzantine!
                Nothing                              -> return Nothing
            Nothing                                  -> checkPrevoteBlock propBlockID
      -- Proposal invalid or absent. Prevote NIL
      | otherwise                          = return Nothing
    --
    checkPrevoteBlock bid = do
      valR <- validateBlock bid
      logger InfoS "Block validation for prevote" valR
      case valR of
        GoodProposal    -> return (Just bid)
        InvalidProposal -> return Nothing
        UnseenProposal  -> return Nothing

-- Unlock upon entering prevote which happens if:
--   * We're already locked
--   * We have polca for round R: lock.R < R < current.R
unlockOnPrevote
  :: TMState alg a
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
  :: (Crypto alg, ConsensusMonad m, MonadLogger m)
  => HeightParameres m alg a
  -> Round
  -> TMState alg a
  -> LogTransitionReason
  -> m (TMState alg a)
enterPrecommit par@HeightParameres{..} r sm@TMState{..} reason = do
  logger InfoS "Entering precommit" $ LogTransition currentH smRound smStep r reason
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
  :: (ConsensusMonad m)
  => HeightParameres m alg a
  -> Signed 'Verified alg (Vote 'PreVote alg a)
  -> TMState alg a
  -> m (TMState alg a)
addPrevote HeightParameres{..} v sm@TMState{..} = do
  announceHasPreVote v
  case addSignedValue (voteRound $ signedValue v) v smPrevotesSet of
    InsertOK votes   -> return sm{ smPrevotesSet = votes }
    InsertDup        -> tranquility
    InsertConflict _ -> misdeed

addPrecommit
  :: (ConsensusMonad m)
  => HeightParameres m alg a
  -> Signed 'Verified alg (Vote 'PreCommit alg a)
  -> TMState alg a
  -> m (TMState alg a)
addPrecommit HeightParameres{..} v sm@TMState{..} = do
  announceHasPreCommit v
  case addSignedValue (voteRound $ signedValue v) v smPrecommitsSet of
    InsertOK votes   -> return sm{ smPrecommitsSet = votes }
    InsertDup        -> tranquility
    InsertConflict _ -> misdeed
