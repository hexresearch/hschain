{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
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
module Thundermint.Blockchain.Internal.Algorithm (
    -- * Data types
    ProposalState(..)
  , Message(..)
  , TMState(..)
  , HeightParameters(..)
    -- * State transitions
  , ConsensusMonad(..)
  , ConsensusM(..)
  , ConsensusResult(..)
  , newHeight
  , tendermintTransition
    -- * Data types used for logging
  , LogTransitionReason(..)
  , LogTransition(..)
  , LogProposal(..)
  , LogCommit(..)
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Catch       (MonadThrow(..))
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.TH       as JSON
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map.Strict     as Map
import qualified Data.HashMap.Strict as HM
import           Katip (Severity(..),sl)
import qualified Katip
import Pipes
import GHC.Generics

import Thundermint.Crypto            ( Crypto, Signed, SignedState(..)
                                     , signedValue, signedKeyInfo
                                     )
import Thundermint.Blockchain.Internal.Types
import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Crypto            (unverifySignature)
import Thundermint.Crypto.Containers
import Thundermint.Logger
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Messages being sent to consensus engine
data Message alg a
  = ProposalMsg    !(Signed (ValidatorIdx alg) 'Verified alg (Proposal alg a))
    -- ^ Incoming proposal
  | PreVoteMsg     !(Signed (ValidatorIdx alg) 'Verified alg (Vote 'PreVote   alg a))
    -- ^ Incoming prevote
  | PreCommitMsg   !(Signed (ValidatorIdx alg) 'Verified alg (Vote 'PreCommit alg a))
    -- ^ Incoming precommit
  | TimeoutMsg     !Timeout
    -- ^ Timeout
  deriving Show

-- | Set of parameters and callbacks for consensus algorithm for given
--   height. These parameters are constant while we're deciding on
--   next block.
data HeightParameters (m :: * -> *) alg a = HeightParameters
  { currentH             :: !Height
    -- ^ Height we're on.
  , currentTime          :: !Time
    -- ^ Time of last block
  , validatorSet         :: !(ValidatorSet alg)
    -- ^ Validator set for current height
  , oldValidatorSet      :: !(Maybe (ValidatorSet alg))
    -- ^ Validator set for previous height. It's used when collecting
    --   stragglers votes
  , validatorKey         :: !(Maybe (PrivValidator alg, ValidatorIdx alg))
    -- ^ Validator key and index in validator set for current round
  , readyCreateBlock     :: !(m Bool)
    -- ^ Returns true if validator is ready to create new block. If
    --   false validator will stay in @NewHeight@ step until it
    --   becomes true.
  , proposerForRound     :: !(Round -> ValidatorIdx alg)
    -- ^ Proposer for given round
  , validateBlock        :: !(BlockID alg a -> m ProposalState)
    -- ^ Request validation of particular block
  , createProposal       :: !(Round -> Maybe (Commit alg a) -> m (BlockID alg a))
    -- ^ Create new proposal block. Block itself should be stored
    --   elsewhere.
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
  { transition'H      :: !Height
  , transition'oldR   :: !Round
  , transition'S      :: !Step
  , transition'R      :: !Round
  , transition'reason :: !LogTransitionReason
  }
JSON.deriveJSON JSON.defaultOptions
  { JSON.fieldLabelModifier = drop 11 } ''LogTransition

instance Katip.ToObject LogTransition
instance Katip.LogItem  LogTransition where
  payloadKeys Katip.V0 _ = Katip.SomeKeys ["H","R","S","newR"]
  payloadKeys _        _ = Katip.AllKeys

-- | Description of proposal
data LogProposal alg a = LogProposal
  { proposal'H   :: !Height
  , proposal'R   :: !Round
  , proposal'bid :: !(BlockID alg a)
  }


instance Katip.ToObject (LogProposal alg a) where
  toObject p = HM.fromList [ ("H",   JSON.toJSON (proposal'H p))
                           , ("R",   JSON.toJSON (proposal'R p))
                           , ("bid", JSON.toJSON $ let BlockID hash = proposal'bid p
                                                   in hash
                             )]
instance Katip.LogItem (LogProposal alg a) where
  payloadKeys Katip.V0 _ = Katip.SomeKeys ["H","R"]
  payloadKeys _        _ = Katip.AllKeys

-- | Description of commit
data LogCommit alg a = LogCommit
  { commit'H   :: !Height
  , commit'bid :: !(BlockID alg a)
  }
instance Katip.ToObject (LogCommit alg a) where
  toObject p = HM.fromList [ ("H",   JSON.toJSON (commit'H p))
                           , ("bid", JSON.toJSON $ let BlockID hash = commit'bid p
                                                   in hash
                             )]
instance Katip.LogItem (LogCommit alg a) where
  payloadKeys _ _ = Katip.AllKeys


-- | Analog of @ExceptT Err IO@
newtype ConsensusM alg a m b = ConsensusM
  { runConsesusM :: m (ConsensusResult alg a b) }
  deriving (Functor)

data ConsensusResult alg a b
  = Success !b
  | Tranquility
  | Misdeed
  | DoCommit  !(Commit alg a) !(TMState alg a)
  deriving (Show,Functor)

instance Monad m => Applicative (ConsensusM alg a m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ConsensusM alg a m) where
  return = ConsensusM . return . Success
  ConsensusM m >>= f = ConsensusM $ m >>= \case
    Success a     -> runConsesusM (f a)
    Tranquility   -> return Tranquility
    Misdeed       -> return Misdeed
    DoCommit cm r -> return $ DoCommit cm r

instance MonadIO m => MonadIO (ConsensusM alg a m) where
  liftIO = ConsensusM . fmap Success . liftIO

instance Monad m => ConsensusMonad (ConsensusM alg a m) where
  tranquility = ConsensusM $ return Tranquility
  misdeed     = ConsensusM $ return Misdeed
  panic       = error

instance MonadLogger m => MonadLogger (ConsensusM alg a m) where
  logger s l a = lift $ logger s l a
  localNamespace f (ConsensusM action) = ConsensusM $ localNamespace f action

instance MonadTrans (ConsensusM alg a) where
  lift = ConsensusM . fmap Success

instance MonadThrow m => MonadThrow (ConsensusM alg a m) where
  throwM = lift . throwM

-- | We're done for this height. Commit block to blockchain
commitBlock :: Monad m => Commit alg a -> TMState alg a -> ConsensusM alg a m x
commitBlock cm r = ConsensusM $ return $ DoCommit cm r


----------------------------------------------------------------
-- Consensus
----------------------------------------------------------------

type CNS x alg a m = ConsensusM alg a (Pipe x (EngineMessage alg a) m)

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
  :: (MonadLogger m)
  => HeightParameters m alg a
  -> Maybe (Commit alg a)
  -> (ConsensusM alg a (Proxy x x' () (EngineMessage alg a) m)) (TMState alg a)
newHeight HeightParameters{..} lastCommit = do
  logger InfoS "Entering new height ----------------" (sl "H" currentH)
  lift $ yield $ EngTimeout $ Timeout  currentH (Round 0) StepNewHeight
  lift $ yield $ EngAnnStep $ FullStep currentH (Round 0) StepNewHeight
  return TMState
    { smRound         = Round 0
    , smStep          = StepNewHeight
    , smProposals     = Map.empty
    , smPrevotesSet   = newHeightVoteSet validatorSet currentTime
    , smPrecommitsSet = newHeightVoteSet validatorSet currentTime
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
  :: (Crypto alg, MonadLogger m)
  => HeightParameters m alg a  -- ^ Parameters for current height
  -> Message alg a             -- ^ Message which causes state transition
  -> TMState alg a             -- ^ Initial state of state machine
  -> CNS x alg a m (TMState alg a)
tendermintTransition par@HeightParameters{..} msg sm@TMState{..} =
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
      | signedKeyInfo p /= proposerForRound propRound
        -> misdeed
      -- Add it to map of proposals
      | otherwise
        -> do logger InfoS "Got proposal" $ LogProposal propHeight propRound propBlockID
              lift $ yield $ EngAcceptBlock propRound propBlockID
              return sm { smProposals = Map.insert propRound p smProposals }
    ----------------------------------------------------------------
    PreVoteMsg v@(signedValue -> Vote{..})
      -- Only accept votes with current height
      | voteHeight /= currentH      -> tranquility
      -- If we awaiting commit we don't care about prevotes
      | StepAwaitCommit _ <- smStep -> tranquility
      | otherwise                   -> checkTransitionPrevote par voteRound
                                   =<< addPrevote par v sm
    ----------------------------------------------------------------
    PreCommitMsg v@(signedValue -> Vote{..})
      -- Collect stragglers precommits for inclusion of
      | smStep == StepNewHeight
      , Just cmt@(Commit cmtID voteList) <- smLastCommit
      , Vote{voteRound = r}              <- signedValue $ NE.head voteList
      , succ voteHeight == currentH
      , voteRound       == r
        -> case voteBlockID of
             -- Virtuous node can either vote for same block or for NIL
             Just bid | bid /= cmtID                  -> misdeed
             -- Add vote while ignoring duplicates
             _        | v' `elem` commitPrecommits cmt -> tranquility
                      | otherwise                      -> return sm
               { smLastCommit = Just cmt { commitPrecommits = NE.cons v' (commitPrecommits cmt) } }
      -- Only accept votes with current height
      | voteHeight /= currentH -> tranquility
      | otherwise              -> checkTransitionPrecommit par voteRound
                              =<< addPrecommit par v sm
      where
        v' = unverifySignature v
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
          StepNewHeight     -> needNewBlock par sm >>= \case
            True  -> enterPropose par smRound sm Reason'Timeout
            False -> do lift $ yield $ EngTimeout $ Timeout currentH (Round 0) StepNewHeight
                        return sm
          StepProposal      -> enterPrevote   par smRound        sm Reason'Timeout
          StepPrevote       -> enterPrecommit par smRound        sm Reason'Timeout
          StepPrecommit     -> enterPropose   par (succ smRound) sm Reason'Timeout
          StepAwaitCommit _ -> tranquility
      where
        t0 = Timeout currentH smRound smStep

-- Check whether we need to create new block or we should wait
needNewBlock
  :: (Monad m)
  => HeightParameters m alg a
  -> TMState alg a
  -> CNS x alg a m Bool
needNewBlock HeightParameters{..} TMState{..}
  -- We want to create first block signed by all validators as soon as
  -- possible
  | currentH == Height 1 = return True
  | otherwise            = lift $ lift readyCreateBlock

-- Check whether we need to perform any state transition after we
-- received prevote
checkTransitionPrevote
  :: (MonadLogger m, Crypto alg)
  => HeightParameters m alg a
  -> Round
  -> TMState alg a
  -> CNS x alg a m (TMState alg a)
checkTransitionPrevote par@HeightParameters{..} r sm@(TMState{..})
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
  :: (MonadLogger m, Crypto alg)
  => HeightParameters m alg a
  -> Round
  -> TMState alg a
  -> CNS x alg a m (TMState alg a)
checkTransitionPrecommit par@HeightParameters{..} r sm@(TMState{..})
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
  | Just (Just bid) <- majority23at r smPrecommitsSet
    = do logger InfoS "Decision to commit" $ LogCommit currentH bid
         lift $ yield $ EngAcceptBlock r bid
         commitBlock Commit{ commitBlockID    = bid
                           , commitPrecommits =  unverifySignature
                                             <$> NE.fromList (valuesAtR r smPrecommitsSet)
                           }
                     sm { smStep = StepAwaitCommit r }
  --  * We have +2/3 precommits for nil at current round
  --  * We are at Precommit step [FIXME?]
  --  => goto Propose(H,R+1)
  | r == smRound
  , Just Nothing <- majority23at r smPrecommitsSet
    = enterPropose par (succ r) sm Reason'PC_Nil
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
  :: (MonadLogger m)
  => HeightParameters m alg a
  -> Round
  -> TMState alg a
  -> LogTransitionReason
  -> CNS x alg a m (TMState alg a)
enterPropose HeightParameters{..} r sm@TMState{..} reason = do
  logger InfoS "Entering propose" $ LogTransition currentH smRound smStep r reason
  lift $ yield $ EngTimeout $ Timeout  currentH r StepProposal
  lift $ yield $ EngAnnStep $ FullStep currentH r StepProposal
  -- If we're proposers we need to broadcast proposal. Otherwise we do
  -- nothing
  when areWeProposers $ case smLockedBlock of
    -- FIXME: take care of POL fields of proposal
    --
    -- If we're locked on block we MUST propose it
    Just (br,bid) -> do logger InfoS "Making POL proposal" $ LogProposal currentH smRound bid
                        lift $ yield $ EngCastPropose r bid (Just br)
    -- Otherwise we need to create new block from mempool
    Nothing      -> do bid <- lift $ lift $ createProposal r smLastCommit
                       logger InfoS "Making new proposal" $ LogProposal currentH smRound bid
                       lift $ yield $ EngCastPropose r bid Nothing
  return sm { smRound = r
            , smStep  = StepProposal
            }
  where
    areWeProposers = Just (proposerForRound r) == fmap snd validatorKey

-- Enter PREVOTE step. Upon entering it we:
--
--   1. Select and broadcast prevote
--   2. Call checkTransitionPrevote to check whether we need to go to
--      PRECOMMIT
enterPrevote
  :: (Crypto alg, MonadLogger m)
  => HeightParameters m alg a
  -> Round
  -> TMState alg a
  -> LogTransitionReason
  -> CNS x alg a m (TMState alg a)
enterPrevote par@HeightParameters{..} r (unlockOnPrevote -> sm@TMState{..}) reason = do
  --
  logger InfoS "Entering prevote" $ LogTransition currentH smRound smStep r reason
  lift $ yield . EngCastPreVote smRound =<< lift prevoteBlock
  --
  lift $ yield $ EngTimeout $ Timeout currentH r StepPrevote
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
            Just lockR ->
              case majority23at lockR smPrevotesSet of
                Just bid
                  | lockR < propRound
                  , bid == Just propBlockID -> checkPrevoteBlock propBlockID
                  | otherwise               -> do
                      --  FIXME: Byzantine!
                      logger WarningS "BYZANTINE proposal POL BID does not match votes" ()
                      return Nothing
                --
                -- FIXME: Here we allow block even if we don't have
                --        POL for the round it's locked on. We need to
                --        do this because in order to perform that
                --        check we need to gossip POL votes with
                --        priority and we don't have support for that
                --        yet
                --
                -- Nothing                               -> return Nothing
                Nothing                               -> checkPrevoteBlock propBlockID
            Nothing                                   -> checkPrevoteBlock propBlockID
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
--   * We have polka for round R: lock.R < R < current.R
unlockOnPrevote
  :: TMState alg a
  -> TMState alg a
unlockOnPrevote sm@TMState{..}
  | Just (lockR, _) <- smLockedBlock
  , hasAnyPolka lockR
    = sm { smLockedBlock = Nothing }
  | otherwise
    = sm
  where
    hasAnyPolka lockR  = not $ null
      [ ()
      | r      <- [succ lockR .. pred smRound]
      , Just _ <- [majority23at r smPrevotesSet]
      ]

-- Enter PRECOMMIT stage.
--   1. Select block to precommits and change lock state if necessary
--   2. Broadcast precommit vote
--   3. Check whether we need to make further state transitions.
enterPrecommit
  :: (Crypto alg, MonadLogger m)
  => HeightParameters m alg a
  -> Round
  -> TMState alg a
  -> LogTransitionReason
  -> CNS x alg a m (TMState alg a)
enterPrecommit par@HeightParameters{..} r sm@TMState{..} reason = do
  logger InfoS "Entering precommit" $ LogTransition currentH smRound smStep r reason
  lift $ yield $ EngCastPreCommit r precommitBlock
  lift $ yield $ EngTimeout $ Timeout currentH r StepPrecommit
  checkTransitionPrecommit par r sm
    { smStep        = StepPrecommit
    , smRound       = r
    , smLockedBlock = lock
    }
  where
    (precommitBlock,lock)
      -- We have polka at round R
      | Just mbid <- majority23at r smPrevotesSet
        = case mbid of
            -- Polka for NIL. Unlock and precommit NIL
            Nothing  -> ( Nothing
                        , Nothing)
            -- Polka for block B. Re-lock to B and precommit it
            Just bid -> ( Just bid
                        , Just (r,bid))
       -- Otherwise keep lock unchanged and precommit NIL
      | otherwise
        = (Nothing, smLockedBlock)


addPrevote
  :: (Monad m)
  => HeightParameters m alg a
  -> Signed (ValidatorIdx alg) 'Verified alg (Vote 'PreVote alg a)
  -> TMState alg a
  -> CNS x alg a m (TMState alg a)
addPrevote HeightParameters{..} v sm@TMState{..} = do
  lift $ yield $ EngAnnPreVote v
  case addSignedValue (voteRound $ signedValue v) v smPrevotesSet of
    InsertOK votes   -> return sm { smPrevotesSet = votes }
    InsertDup        -> tranquility
    InsertConflict _ -> misdeed
    -- NOTE: Couldn't happen since we reject votes signed by unknown keys
    InsertUnknown  _ -> error "addPrevote: Internal error"

addPrecommit
  :: (Monad m)
  => HeightParameters m alg a
  -> Signed (ValidatorIdx alg) 'Verified alg (Vote 'PreCommit alg a)
  -> TMState alg a
  -> CNS x alg a m (TMState alg a)
addPrecommit HeightParameters{..} v sm@TMState{..} = do
  lift $ yield $ EngAnnPreCommit v
  case addSignedValue (voteRound $ signedValue v) v smPrecommitsSet of
    InsertOK votes   -> return sm { smPrecommitsSet = votes }
    InsertDup        -> tranquility
    InsertConflict _ -> misdeed
    -- NOTE: See addPrevote
    InsertUnknown  _ -> error "addPrecommit: Internal error"
