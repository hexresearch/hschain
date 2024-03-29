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
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
-- |
-- Implementation of tendermint consensus protocol.
--
-- Note that this
-- module provides only implementation of consensus state machine and
-- assumes that everything sent to it is already validated! In
-- particular dealing with block content is delegated to other parts
-- of library
module HSChain.Blockchain.Internal.Algorithm (
    -- * Data types
    ProposalState(..)
  , Message(..)
  , TMState(..)
  , HeightParameters(..)
    -- * State transitions
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
import           Katip (sl)
import qualified Katip
import Pipes
import GHC.Generics

import HSChain.Crypto            ( Crypto, SignedState(..) )
import HSChain.Blockchain.Internal.Types
import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Crypto.Containers
import HSChain.Internal.Types.Consensus
import HSChain.Internal.Types.Messages
import HSChain.Logger
import HSChain.Store.Internal.Proposals
import HSChain.Types


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Messages being sent to consensus engine
data Message a
  = ProposalMsg    !(Signed 'Verified (Alg a) (Proposal a))
    -- ^ Incoming proposal
  | PreVoteMsg     !(Signed 'Verified (Alg a) (Vote 'PreVote   a))
    -- ^ Incoming prevote
  | PreCommitMsg   !(Signed 'Verified (Alg a) (Vote 'PreCommit a))
    -- ^ Incoming precommit
  | TimeoutMsg     !Timeout
    -- ^ Timeout
  deriving Show

-- | Set of parameters and callbacks for consensus algorithm for given
--   height. These parameters are constant while we're deciding on
--   next block.
data HeightParameters m view = HeightParameters
  { currentH         :: !Height
    -- ^ Height we're on.
  , hValidatorSet    :: !(ValidatorSet (AlgOf view))
    -- ^ Validator set for current height
  , oldValidatorSet  :: !(ValidatorSet (AlgOf view))
    -- ^ Validator set for previous height
  , validatorKey     :: !(Maybe ( PrivValidator (AlgOf view)
                                , ValidatorIdx (AlgOf view)))
    -- ^ Validator key and index in validator set for current round
  , readyCreateBlock :: Int -> m Bool
    -- ^ Returns true if validator is ready to create new block. If
    --   false validator will stay in @NewHeight@ step until it
    --   becomes true.
  , proposerForRound :: Round
                     -> ValidatorIdx (AlgOf view)
    -- ^ Proposer for given round
  , validateBlock    :: Props view
                     -> BlockIdOf view
                     -> m ( Props view -> Props view
                          , ProposalState)
    -- ^ Request validation of particular block
  , createProposal   :: Round
                     -> Maybe (Commit (BlockType view))
                     -> m ( Props view -> Props view
                          , BlockIdOf view)
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
data LogProposal a = LogProposal
  { proposal'H   :: !Height
  , proposal'R   :: !Round
  , proposal'bid :: !(BlockID a)
  }


instance Katip.ToObject (LogProposal a) where
  toObject p = HM.fromList [ ("H",   JSON.toJSON (proposal'H p))
                           , ("R",   JSON.toJSON (proposal'R p))
                           , ("bid", JSON.toJSON $ let BlockID hash = proposal'bid p
                                                   in hash
                             )]
instance Katip.LogItem (LogProposal a) where
  payloadKeys Katip.V0 _ = Katip.SomeKeys ["H","R"]
  payloadKeys _        _ = Katip.AllKeys

-- | Description of commit
data LogCommit a = LogCommit
  { commit'H   :: !Height
  , commit'bid :: !(BlockID a)
  }
instance Katip.ToObject (LogCommit a) where
  toObject p = HM.fromList [ ("H",   JSON.toJSON (commit'H p))
                           , ("bid", JSON.toJSON $ let BlockID hash = commit'bid p
                                                   in hash
                             )]
instance Katip.LogItem (LogCommit a) where
  payloadKeys _ _ = Katip.AllKeys


-- | Analog of @ExceptT Err IO@
newtype ConsensusM view m x = ConsensusM
  { runConsesusM :: m (ConsensusResult view x) }
  deriving (Functor)

data ConsensusResult view a
  = Success !a
  | Tranquility
  | Misdeed
  | DoCommit  !(Commit (BlockType view)) !(TMState view)
  deriving (Functor)

instance Monad m => Applicative (ConsensusM view m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ConsensusM view m) where
  return = ConsensusM . return . Success
  ConsensusM m >>= f = ConsensusM $ m >>= \case
    Success a     -> runConsesusM (f a)
    Tranquility   -> return Tranquility
    Misdeed       -> return Misdeed
    DoCommit cm r -> return $ DoCommit cm r

instance MonadIO m => MonadIO (ConsensusM view m) where
  liftIO = ConsensusM . fmap Success . liftIO

instance MonadLogger m => MonadLogger (ConsensusM view m) where
  logger s l a = lift $ logger s l a
  localNamespace f (ConsensusM action) = ConsensusM $ localNamespace f action

instance MonadTrans (ConsensusM view) where
  lift = ConsensusM . fmap Success

instance MonadThrow m => MonadThrow (ConsensusM view m) where
  throwM = lift . throwM

-- | We're done for this height. Commit block to blockchain
commitBlock :: (Monad m, a ~ BlockType view)
            => Commit a -> TMState view -> ConsensusM view m x
commitBlock cm r = ConsensusM $ return $ DoCommit cm r


----------------------------------------------------------------
-- Consensus
----------------------------------------------------------------

type CNS x view m = ConsensusM view (Pipe x (EngineMessage (BlockType view)) m)

-- | Message had no effect and was ignored. No change to state
--   happened and execution is aborted.
tranquility :: Monad m => ConsensusM view m x
tranquility = ConsensusM $ return Tranquility

-- | We detected clearly malicious behavior.
misdeed :: (Monad m, a ~ BlockType view) => [ByzantineEvidence a] -> CNS x view m y
misdeed es = do
  lift $ mapM_ (yield . EngMisdeed) es
  ConsensusM $ return Misdeed


-- | Enter new height and create new state for state machine
newHeight
  :: (MonadLogger m, a ~ BlockType view)
  => HeightParameters m view
  -> Maybe (Commit a)
  -> Producer (EngineMessage a) m (TMState view)
newHeight HeightParameters{..} lastCommit = do
  lift $ logger InfoS "Entering new height ----------------" (sl "H" currentH)
  yield $ EngTimeout $ Timeout  currentH (Round 0) (StepNewHeight 0)
  yield $ EngAnnStep $ FullStep currentH (Round 0) (StepNewHeight 0)
  return TMState
    { smRound          = Round 0
    , smStep           = StepNewHeight 0
    , smProposals      = Map.empty
    , smProposedBlocks = emptyProps
    , smPrevotesSet    = newHeightVoteSet hValidatorSet
    , smPrecommitsSet  = newHeightVoteSet hValidatorSet
    , smLockedBlock    = Nothing
    , smLastCommit     = lastCommit
    }

-- | Transition rule for tendermint state machine. State is passed
--   explicitly and we track effects like sending message and
--   committing in the monad.
--
--   Note that when state machine sends vote or proposal it does not
--   update state and thus message should be send back to it
tendermintTransition
  :: (Crypto (Alg a), MonadLogger m, a ~ BlockType view)
  => HeightParameters m view  -- ^ Parameters for current height
  -> Message a                -- ^ Message which causes state transition
  -> TMState view             -- ^ Initial state of state machine
  -> CNS x view m (TMState view)
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
      --        implementation have same question (#313)
      | Just _ <- Map.lookup propRound smProposals
        -> tranquility
      -- Node sending message out of order is clearly byzantine
      | signedKeyInfo p /= proposerForRound propRound
        -> misdeed [OutOfTurnProposal $ unverifySignature p]
      -- Add it to map of proposals
      | otherwise
        -> do lift $ lift $ logger InfoS "Got proposal" $ LogProposal propHeight propRound propBlockID
              lift $ yield $ EngAnnProposal propRound
              return sm { smProposals      = Map.insert propRound p smProposals
                        , smProposedBlocks = acceptBlockID propRound propBlockID smProposedBlocks
                        }
    ----------------------------------------------------------------
    PreVoteMsg v@(signedValue -> Vote{..})
      -- Only accept votes with current height
      | voteHeight /= currentH      -> tranquility
      -- If we awaiting commit we don't care about prevotes
      | StepAwaitCommit _ <- smStep -> tranquility
      | otherwise                   -> checkTransitionPrevote par voteRound
                                   =<< addPrevote v sm
    ----------------------------------------------------------------
    PreCommitMsg v@(signedValue -> Vote{..})
      -- Collect stragglers precommits for inclusion of
      | StepNewHeight _                  <- smStep
      , Just cmt@(Commit cmtID voteList) <- smLastCommit
      , Vote{voteRound = r}              <- signedValue $ NE.head voteList
      , succ voteHeight == currentH
      , voteRound       == r
        -> case voteBlockID of
             -- Virtuous node can either vote for same block or for NIL
             Just bid | bid /= cmtID                   -> misdeed []
             -- Add vote while ignoring duplicates
             _        | v' `elem` commitPrecommits cmt -> tranquility
                      | otherwise                      -> return sm
               { smLastCommit = Just cmt { commitPrecommits = NE.cons v' (commitPrecommits cmt) } }
      -- Only accept votes with current height
      | voteHeight /= currentH -> tranquility
      | otherwise              -> checkTransitionPrecommit par voteRound
                              =<< addPrecommit v sm
      where
        v' = unverifySignature v
    ----------------------------------------------------------------
    TimeoutMsg t ->
      case t `compare` t0 of
        -- It's timeout from previous steps. Ignore it
        LT -> tranquility
        -- Timeout from future. Must not happen
        GT -> error $ "Timeout from future: " ++ show (t,t0)
        -- Update state accordingly. We unconditionally enter next step of
        -- round or next round.
        EQ -> case smStep of
          StepNewHeight n   -> canCreate par n >>= \case
            True  -> enterPropose par smRound sm Reason'Timeout
            False -> do let step = StepNewHeight (n+1)
                        lift $ yield $ EngTimeout $ Timeout currentH (Round 0) step
                        return sm { smStep = step }
          StepProposal      -> enterPrevote   par smRound        sm Reason'Timeout
          StepPrevote       -> enterPrecommit par smRound        sm Reason'Timeout
          StepPrecommit     -> enterPropose   par (succ smRound) sm Reason'Timeout
          StepAwaitCommit _ -> tranquility
      where
        t0 = Timeout currentH smRound smStep

-- Check whether we need to create new block or we should wait
canCreate
  :: (Monad m)
  => HeightParameters m a
  -> Int
  -> CNS x a m Bool
canCreate HeightParameters{..} n
  -- We want to create first block signed by all validators as soon as
  -- possible
  | currentH == Height 1 = return True
  | otherwise            = lift $ lift $ readyCreateBlock n

-- Check whether we need to perform any state transition after we
-- received prevote
checkTransitionPrevote
  :: (MonadLogger m, Crypto (AlgOf view))
  => HeightParameters m view
  -> Round
  -> TMState view
  -> CNS x view m (TMState view)
checkTransitionPrevote par r sm@TMState{..}
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
  :: (MonadLogger m, Crypto (AlgOf view))
  => HeightParameters m view
  -> Round
  -> TMState view
  -> CNS x view m (TMState view)
checkTransitionPrecommit par@HeightParameters{..} r sm@TMState{..}
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
    = do lift $ lift $ logger InfoS "Decision to commit" $ LogCommit currentH bid
         lift $ yield $ EngAnnProposal r
         commitBlock Commit{ commitBlockID    = bid
                           , commitPrecommits =  unverifySignature
                                             <$> NE.fromList (valuesAtR r smPrecommitsSet)
                           }
                     sm { smStep           = StepAwaitCommit r
                        , smProposedBlocks = acceptBlockID r bid smProposedBlocks
                        }
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
  => HeightParameters m view
  -> Round
  -> TMState view
  -> LogTransitionReason
  -> CNS x view m (TMState view)
enterPropose HeightParameters{..} r sm@TMState{..} reason = do
  lift $ lift $ logger InfoS "Entering propose" $ LogTransition currentH smRound smStep r reason
  lift $ yield $ EngTimeout $ Timeout  currentH r StepProposal
  lift $ yield $ EngAnnStep $ FullStep currentH r StepProposal
  -- If we're proposers we need to broadcast proposal. Otherwise we do
  -- nothing
  propUpdate <- case (areWeProposers, smLockedBlock) of    
    -- If we're locked on block we MUST propose it
    (True, Just (br,bid)) -> do
      lift $ lift $ logger InfoS "Making POL proposal" $ LogProposal currentH smRound bid
      lift $ yield $ EngCastPropose r bid (Just br)
      return id
    -- Otherwise we need to create new block from mempool
    (True, Nothing) -> do
      (upd,bid) <- lift $ lift $ createProposal r smLastCommit
      lift $ lift $ logger InfoS "Making new proposal" $ LogProposal currentH r bid
      lift $ yield $ EngCastPropose r bid Nothing
      return upd
    -- We aren't proposers. Do nothing
    _ -> return id
  return sm { smRound          = r
            , smStep           = StepProposal
            , smProposedBlocks = propUpdate smProposedBlocks
            }
  where
    areWeProposers = Just (proposerForRound r) == fmap snd validatorKey

-- Enter PREVOTE step. Upon entering it we:
--
--   1. Select and broadcast prevote
--   2. Call checkTransitionPrevote to check whether we need to go to
--      PRECOMMIT
enterPrevote
  :: (Crypto (AlgOf view), MonadLogger m)
  => HeightParameters m view
  -> Round
  -> TMState view
  -> LogTransitionReason
  -> CNS x view m (TMState view)
enterPrevote par@HeightParameters{..} r (unlockOnPrevote -> sm@TMState{..}) reason = do
  --
  lift $ lift $ logger InfoS "Entering prevote" $ LogTransition currentH smRound smStep r reason
  (updateProp, bid) <- lift $ lift prevoteBlock
  lift $ yield $ EngCastPreVote smRound bid
  --
  lift $ yield $ EngTimeout $ Timeout currentH r StepPrevote
  checkTransitionPrevote par r sm
    { smRound          = r
    , smStep           = StepPrevote
    , smProposedBlocks = updateProp smProposedBlocks
    }
  where
    prevoteBlock
      -- We're locked on block so we prevote it
      | Just (_,bid) <- smLockedBlock = return (id, Just bid)
      -- We have proposal. Prevote it if it's good
      | Just (signedValue -> Proposal{..}) <- Map.lookup r smProposals =
          -- If proposal have proof of lock we must have proof
          -- of lock for same round and some block
          case propPOL of
            Nothing    -> checkPrevoteBlock propBlockID
            Just lockR ->
              case majority23at lockR smPrevotesSet of
                Just bid
                  | lockR < propRound
                  , bid == Just propBlockID -> checkPrevoteBlock propBlockID
                  | otherwise               -> do
                      logger WarningS "BYZANTINE proposal POL BID does not match votes" ()
                      return (id, Nothing)
                Nothing -> return (id, Nothing)
      -- Proposal invalid or absent. Prevote NIL
      | otherwise = return (id, Nothing)
    --
    checkPrevoteBlock bid = do
      (upd,valR) <- validateBlock smProposedBlocks bid
      logger InfoS "Block validation for prevote" valR
      case valR of
        GoodProposal      -> return (upd, Just bid)
        InvalidProposal _ -> return (upd, Nothing)
        UnseenProposal    -> return (upd, Nothing)

-- Unlock upon entering prevote which happens if:
--   * We're already locked
--   * We have polka for round R: lock.R < R < current.R
unlockOnPrevote
  :: TMState view
  -> TMState view
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
  :: (Crypto (AlgOf view), MonadLogger m)
  => HeightParameters m view
  -> Round
  -> TMState view
  -> LogTransitionReason
  -> CNS x view m (TMState view)
enterPrecommit par@HeightParameters{..} r sm@TMState{..} reason = do
  lift $ lift $ logger InfoS "Entering precommit" $ LogTransition currentH smRound smStep r reason
  lift $ yield $ EngCastPreCommit r precommitBlock
  lift $ yield $ EngTimeout $ Timeout currentH r StepPrecommit
  lift $ yield $ EngAnnLock $ fst <$> lock
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
  :: (Monad m, a ~ BlockType view)
  => Signed 'Verified (Alg a) (Vote 'PreVote a)
  -> TMState view
  -> CNS x view m (TMState view)
addPrevote v@(signedValue -> Vote{..}) sm@TMState{..} = do
  lift $ yield $ EngAnnPreVote v
  case addSignedValue voteRound v smPrevotesSet of
    InsertOK votes   -> return sm { smPrevotesSet = votes }
    InsertDup        -> tranquility
    InsertConflict u -> misdeed
                      [ ConflictingPreVote (unverifySignature v) (unverifySignature u)]
    -- NOTE: Couldn't happen since we reject votes signed by unknown keys
    InsertUnknown    -> error "addPrevote: Internal error"

addPrecommit
  :: (Monad m, a ~ BlockType view)
  => Signed 'Verified (Alg a) (Vote 'PreCommit a)
  -> TMState view
  -> CNS x view m (TMState view)
addPrecommit v@(signedValue -> Vote{..}) sm@TMState{..} = do
  lift $ yield $ EngAnnPreCommit v
  case addSignedValue voteRound v smPrecommitsSet of
    InsertOK votes   -> return sm { smPrecommitsSet = votes }
    InsertDup        -> tranquility
    InsertConflict u -> misdeed
                      [ ConflictingPreCommit (unverifySignature v) (unverifySignature u)]
    -- NOTE: See addPrevote
    InsertUnknown    -> error "addPrecommit: Internal error"
