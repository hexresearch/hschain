-- |Implementing thundermint as parsing over
-- stream of events, hence "Lightweight ThunderMint".
--

{-# LANGUAGE DeriveAnyClass, DeriveFunctor, FlexibleContexts, GADTs #-}
{-# LANGUAGE RecordWildCards, StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module LTM.LTM where

import Control.Monad (when, forM, forM_)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.Printf

import LTM.SP

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

  -- |Form the block from state, modifying state in the process.
  -- Nothing means we aren't proposing.
  proposal :: state -> Maybe (Block state, [OutputMessage state], state)

  -- |Add a vote from ourselves.
  addSelfVote :: Maybe (Block state) -> state -> state

  -- |Get a list of peer identifiers from state.
  peersList :: state -> [PeerId state]

  -- |Timeout for propose, microseconds.
  halfRoundTime :: state -> Microseconds

  -- |Receive a vote. A processor that
  -- awaits for input messages and returns
  -- voting messages - pairs (peer ID, Vote).
  -- State parameter is used as witness.
  receiveVote :: state -> SP (InputMessage state) (PeerId state, Vote)

  -- 

type LTMSP state a = SPB (LTMIn state) (LTMOut state) a

type LTM state = LTMSP state ()

lightweightThundermint :: LTMState state
                       => state
                       -> LTM state
lightweightThundermint state = loop state
  where
    loop state = do
      (state, block) <- proposeReceive state
      (state, block) <- precommit state block
      state <- commit state block
      loop state

proposeReceive :: LTMState state => state -> LTMSP state (state, Maybe (Block state))
proposeReceive state = withTimeout (halfRoundTime state) $ do
  case proposal state of
    Just (block, blockMessages, state') -> do
      forM_ blockMessages outMessage
      collect (addSelfVote (Just block) state') (Just block)
    Nothing -> collect state Nothing
  where
    collect state maybeBlock = error "collect!"

outMessage :: OutputMessage state -> LTMSP state ()
outMessage msg = out $ OutMessage msg

withTimeout :: Microseconds -> LTMSP state a -> LTMSP state a
withTimeout waitFor processor = do
  error "with timeout"

precommit = error "pcm"
commit state = error "comt"

{-

-- |Send proposa
proposeReceive :: state
               -> SPB (Timed (InputMessage state)) (UpDown state (OutputMessage state)) ()
proposeReceive height round state = do
  state' <- if shouldPropose state
    then do
      let (block, state') = proposal state
      multicast block state'
      return state'
    else return state
  withTimeout roundTimeout state' PreVote $ do
    scanSP selectProposal state' (receiveVote state')

selectProposal :: a
selectProposal = undefined

multicast :: Block state -> state -> SP a (OutputMessage state)
multicast block state = error "multicast!"

withTimeout :: (state -> Microseconds)
            -> state
            -> VotingStage
            -> SP (InputMessage state) state
            -> SP (UpDown () ()) ()
withTimeout us state stage belowProcessor =
  undefined
-}

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

  receiveVote = undefined

  addSelfVote maybeBlock state@TestState{..} =
    state { testStateVotesForBlock = Map.insertWith Set.union maybeBlock (Set.singleton testStateId) testStateVotesForBlock }

testIds :: [String]
testIds = ["n-" ++ printf "%03d" (i + 1) | i <- [0 :: Int .. 3]]

testLTMMap :: Map.Map String (LTM TestState)
testLTMMap = Map.fromList [(i, lightweightThundermint $ emptyTestState i testIds) | i <- testIds]

findSomeOuts processors = searchOut [] [] $ Map.toList processors
  where
    searchOut outputs acc [] = (outputs, Map.fromAscList $ reverse acc)
    searchOut outputs acc ((i, ltm) : iltms) = case ltm of
      SPB (O o sp) -> searchOut ((i, o):outputs) acc ((i, SPB sp) : iltms)
      _ -> searchOut outputs ((i,ltm) : acc) iltms

run :: [Either (LTMOut TestState) ()] -> [Either (LTMOut TestState) ()]
    -> Map.Map String (LTM TestState) -> [(String, Either (LTMOut TestState) ())]
run [] [] processors = case findSomeOuts processors of
  ([], procs) -> error "no messages and no outputs!"
  (os, procs) -> os ++ run [] (map snd os) procs

t = run [] [] testLTMMap
