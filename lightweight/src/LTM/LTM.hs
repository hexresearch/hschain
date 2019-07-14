-- |Implementing thundermint as parsing over
-- stream of events, hence "Lightweight ThunderMint".
--

{-# LANGUAGE DeriveAnyClass, DeriveFunctor, FlexibleContexts, GADTs #-}
{-# LANGUAGE RecordWildCards, StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module LTM.LTM where

import Control.Applicative
import Control.Monad (when, forM, forM_)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.Printf

import LTM.SP

import qualified Debug.Trace as DT

type Height = Int
type Round = Int
type Microseconds = Int

data Vote = VoteFor | VoteAgainst deriving (Eq, Ord, Show)

data VotingStage = PreVote | PreCommit deriving (Eq, Ord, Show)

data LTMIn state =
    Timeout Height Round VotingStage
  | InMessage (InputMessage state)

deriving instance (Eq (InputMessage state)) => Eq (LTMIn state)
deriving instance (Ord (InputMessage state)) => Ord (LTMIn state)
deriving instance (Show (InputMessage state)) => Show (LTMIn state)

data LTMOut state =
    RequestTimeout Height Round VotingStage Microseconds
  | OutMessage (OutputMessage state)
  | NewHeight state

deriving instance (Eq (OutputMessage state), Eq state) => Eq (LTMOut state)
deriving instance (Ord (OutputMessage state), Ord state) => Ord (LTMOut state)
deriving instance (Show (OutputMessage state), Show state) => Show (LTMOut state)

class LTMState state where

  -- |Type that identifies peers.
  type PeerId state :: *

  -- |Type of block that incorporates state changes.
  type Block state :: *

  -- |Type of incoming messages.
  type InputMessage state :: *

  -- |Type of outgoing messages.
  type OutputMessage state :: *

  -- |Get current height and round.
  getHeightRound :: state -> (Height, Round)

  -- |Form the block from state, modifying state in the process.
  -- Nothing means we aren't proposing.
  proposal :: state -> Maybe (Block state, [OutputMessage state], state)

  -- |Add a vote from ourselves.
  addSelfVote :: Maybe (Block state) -> state -> state

  -- |Collect a vote from message.
  addVote :: InputMessage state -> state -> ([OutputMessage state], state)

  -- |Are there enough votes so we can stop collecting them?
  enoughVotes :: state -> Bool

  -- |Get a list of peer identifiers from state.
  peersList :: state -> [PeerId state]

  -- |Timeout for propose, microseconds.
  halfRoundTime :: state -> Microseconds

  -- |Get block consent.
  getBlockConsent :: state -> Maybe (Block state)

  -- |Clear votes.
  clearVotes :: state -> state

type LTMSP state a = SP (LTMIn state) (Either (LTMOut state) a)

type LTM state = LTMSP state ()

lightweightThundermint :: LTMState state
                       => state
                       -> LTM state
lightweightThundermint = loop
  where
    loop state =
      proposeReceive state
      >>=^ \(state, block) -> precommit state block
      >>=^ \state -> commit state block
      >>=^ loop

proposeReceive :: LTMState state => state -> LTMSP state (state, Maybe (Block state))
proposeReceive state = withTimeout state halfRoundTime PreVote $ do
  propose state
  >>=^ \state' -> collect state'
  where
    (h, r) = getHeightRound state
    propose state = case proposal state of
      Just (block, blockMessages, state') ->
        forSPE_ blockMessages outMessage
        >>=^ \_ -> returnR (addSelfVote (Just block) $ clearVotes state')
      Nothing -> returnR state
    collect state
      | enoughVotes state = returnR (state, getBlockConsent state)
      | otherwise = inputR
         >>=^ \msg -> case msg of
          InMessage inMsg -> let (messages, state') = addVote inMsg state
            in forSPE_ messages outMessage
               >>^ collect state'
          _ -> collect state

outMessage :: OutputMessage state -> LTMSP state ()
outMessage msg = out $ OutMessage msg

withTimeout :: LTMState state => state -> (state -> Microseconds) -> VotingStage -> LTMSP state a -> LTMSP state a
withTimeout state getWaitTime votingStage processor = do
  out $ RequestTimeout h r votingStage (getWaitTime state)
  loop processor
  where
    (h, r) = getHeightRound state
    loop processor = do
      processor

returnR :: a -> SP i (Either o a)
returnR = return . Right

inputR :: SPE i o i
inputR = fmap Right input

precommit state block = returnR state

commit :: state -> Maybe (Block state) -> LTMSP state state
commit state block =
  out (NewHeight state) >>=^ const (returnR state)

--------------------------------------------------------------------------------
-- tests.

data TSMsg =
    TSProposal  (Block TestState)
  | TSPrevote   (Maybe (Block TestState))
  | TSPrecommit (Maybe (Block TestState))
  deriving (Eq, Ord, Show)

data TestState = TestState
  { testStateHeight        :: Height
  , testStateRound         :: Round
  , testStateId            :: String
  , testStatePeers         :: [String]
  , testStateState         :: [String] -- strings received.
  , testStateWaitingMsgs   :: [String] -- strings waiting to be sent.
  , testStateVotesForBlock :: Map.Map (Maybe (Block TestState)) (Set.Set (PeerId TestState))
  } deriving Show

emptyTestState :: String -> [String] -> TestState
emptyTestState id allIds = TestState
  { testStateHeight        = 0
  , testStateRound         = 0
  , testStateId            = id
  , testStatePeers         = allIds
  , testStateState         = []
  , testStateWaitingMsgs   = []
  , testStateVotesForBlock = Map.empty
  }

instance LTMState TestState where
  type PeerId TestState = String
  type OutputMessage TestState = (PeerId TestState, PeerId TestState, TSMsg)
  type InputMessage TestState = Either (OutputMessage TestState) [String]
  type Block TestState = (PeerId TestState, Height, [String])

  getHeightRound state@TestState{..} = (testStateHeight, testStateRound)

  proposal state@TestState{..}
    | mod (testStateHeight + testStateRound) n == ourIndex =
      Just
        ( block
        , [(dest, testStateId, TSProposal block) | dest <- testStatePeers, dest /= testStateId]
        , state { testStateWaitingMsgs = drop 1 testStateWaitingMsgs })
    | otherwise = Nothing
    where
      n = length testStatePeers
      ourIndex = length $ takeWhile (/= testStateId) testStatePeers
      block = (testStateId, testStateHeight, take 1 testStateWaitingMsgs)

  peersList state@TestState{..} = testStatePeers

  halfRoundTime _state = 100000

  enoughVotes TestState{..} = sum (map Set.size $ Map.elems testStateVotesForBlock) > threshold
    where
      peerCount = length testStatePeers
      threshold = div (peerCount * 2) 3

  addSelfVote maybeBlock state@TestState{..} =
    state { testStateVotesForBlock = Map.insertWith Set.union maybeBlock (Set.singleton testStateId) testStateVotesForBlock }

  addVote (Left (us, them, TSProposal block)) state@TestState {..}
    = (map (\it->(it, us, TSPrevote (Just block))) testStatePeers, state { testStateVotesForBlock = Map.insertWith Set.union (Just block) (Set.singleton them) testStateVotesForBlock })
  addVote (Left (us, them, TSPrevote maybeBlock)) state@TestState {..}
    = ([], state { testStateVotesForBlock = Map.insertWith Set.union maybeBlock (Set.singleton them) testStateVotesForBlock })
  addVote (Right sortaTransactions) state@TestState {..}
    = ([], state { testStateWaitingMsgs = testStateWaitingMsgs ++ sortaTransactions })

  getBlockConsent state@TestState{..} = head (Map.keys consent ++ [Nothing])
    where
      peerCount = length testStatePeers
      threshold = div (peerCount * 2) 3
      consent = Map.filter ((>threshold) . Set.size) testStateVotesForBlock
  clearVotes state = state { testStateVotesForBlock = Map.empty }

testIds :: [String]
testIds = ["n-" ++ printf "%03d" (i + 1) | i <- [0 :: Int .. 3]]

testLTMMap :: Map.Map String (LTM TestState)
testLTMMap = Map.fromList [(i, lightweightThundermint $ emptyTestState i testIds) | i <- testIds]

findSomeOuts processors = searchOut [] [] $ Map.toList processors
  where
    searchOut outputs acc [] = (outputs, Map.fromAscList $ reverse acc)
    searchOut outputs acc ((i, ltm) : iltms) = case ltm of
      O o sp -> searchOut ((i, o):outputs) acc ((i, sp) : iltms)
      _ -> searchOut outputs ((i,ltm) : acc) iltms

run :: [Either (LTMOut TestState) ()] -> [Either (LTMOut TestState) ()]
    -> Map.Map String (LTM TestState) -> [(String, Either (LTMOut TestState) ())]
run [] [] processors = case findSomeOuts processors of
  ([], procs)
    | not (null stopped) -> error $ "stopped: "++show stopped
    | otherwise -> error "no messages and no outputs!"
  (os, procs) -> os ++ run [] (map snd os) procs
  where
    stopped = [ i | (i, N) <- Map.toList processors]
run acc (msg : msgs) processors = case msg of
  Right _ -> error "there must not be Right seen"
  -- no notion of time right now.
  Left (RequestTimeout _ _ _ _) -> run acc msgs processors
  -- new height is purely bureacratic for us.
  Left (NewHeight _) -> run acc msgs processors
  Left (OutMessage om@(dest, src, _)) -> case processor of
    N -> error "definitely an internal error"
    I f -> let sp = f (InMessage $ Left om) in run acc msgs $ Map.insert dest sp processors
    O o sp -> (dest, o) : run acc (msg : (msgs ++ [o])) (Map.insert dest sp processors)
    where
      processor = Map.findWithDefault (error "no processor?") dest processors

t = run [] [] testLTMMap
