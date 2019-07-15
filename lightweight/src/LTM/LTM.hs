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

import System.IO

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
  | InMessage Height Round VotingStage (InputMessage state)

deriving instance (Eq (InputMessage state)) => Eq (LTMIn state)
deriving instance (Ord (InputMessage state)) => Ord (LTMIn state)
deriving instance (Show (InputMessage state)) => Show (LTMIn state)

data LTMOut state =
    RequestTimeout Height Round VotingStage Microseconds
  | OutMessage Height Round VotingStage (OutputMessage state)
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

  -- |Messages we have to send during precommit
  precommitMessages :: state -> [OutputMessage state]

  -- |Apply block to a state. Increases height, sets round to 0.
  applyBlock :: state -> Block state -> state

  -- |Enter new round.
  newRound :: state -> state

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
      >>=^ \state -> commit state
      -- >>^  returnR ()
      >>=^ loop

proposeReceive :: LTMState state => state -> LTMSP state (state, Maybe (Block state))
proposeReceive state =
  out (RequestTimeout h r PreVote $ halfRoundTime  state)
  >>^             propose state
  >>=^ \state' -> collect PreVote state'
  >>=^ \state'' -> returnR (state'', getBlockConsent state'')
  where
    (h, r) = getHeightRound state
    propose state = case proposal state of
      Just (block, blockMessages, state') ->
        forSPE_ blockMessages (outMessage h r PreVote)
        >>=^ \_ -> returnR (addSelfVote (Just block) $ clearVotes state')
      Nothing -> returnR (clearVotes state)

collect :: LTMState state => VotingStage -> state -> LTMSP state (state)
collect vs state
  | enoughVotes state = returnR (state)
  | otherwise = inputR
     >>=^ \msg -> case msg of
      InMessage h' r' vs' inMsg
        | h' == h && r' == r && vs' == vs ->
          let (messages, state') = addVote inMsg state
          in forSPE_ messages (outMessage h r PreVote)
             >>^ collect vs state'
      Timeout h' r' vs'
        | h' == h && r' == r && vs' == vs -> returnR (state)
      _ -> collect vs state
  where
    (h, r) = getHeightRound state

outMessage :: Height -> Round -> VotingStage -> OutputMessage state -> LTMSP state ()
outMessage h r vs msg = out $ OutMessage h r vs msg

returnR :: a -> SP i (Either o a)
returnR = return . Right

inputR :: SPE i o i
inputR = fmap Right input

precommit state block =
  out (RequestTimeout h r PreCommit $ halfRoundTime  state)
  >>^ forSPE_ (precommitMessages state) (out . OutMessage h r PreCommit)
  >>^ collect PreCommit (addSelfVote block $ clearVotes state)
  where
    (h, r) = getHeightRound state

commit :: LTMState state => state ->  LTMSP state state
commit state = case getBlockConsent state of
  Just block -> let state' = applyBlock state block
                in out (NewHeight state') >>^ returnR state'
  Nothing -> returnR (newRound state)

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

  precommitMessages state@TestState{..} =
    [(them, testStateId, TSPrecommit maybeBlock) | them <- testStatePeers, them /= testStateId]
    where
      maybeBlock = getBlockConsent state

  peersList state@TestState{..} = testStatePeers

  halfRoundTime _state = 100000

  enoughVotes TestState{..} = sum (map Set.size $ Map.elems testStateVotesForBlock) > threshold
    where
      peerCount = length testStatePeers
      threshold = div (peerCount * 2) 3

  addSelfVote maybeBlock state@TestState{..} =
    state { testStateVotesForBlock = Map.insertWith Set.union maybeBlock (Set.singleton testStateId) testStateVotesForBlock }

  addVote (Left (us, them, TSProposal block)) state@TestState {..}
    = ([(them, us, TSPrevote (Just block)) | them <- testStatePeers, them /= us]
      , state { testStateVotesForBlock = Map.insertWith Set.union (Just block) (Set.singleton them) testStateVotesForBlock }
      )
  addVote (Left (us, them, TSPrevote maybeBlock)) state@TestState {..}
    = ([], state { testStateVotesForBlock = Map.insertWith Set.union maybeBlock (Set.singleton them) testStateVotesForBlock })
  addVote (Left (us, them, TSPrecommit maybeBlock)) state@TestState {..}
    = ([], state { testStateVotesForBlock = Map.insertWith Set.union maybeBlock (Set.singleton them) testStateVotesForBlock })
  addVote (Right sortaTransactions) state@TestState {..}
    = ([], state { testStateWaitingMsgs = testStateWaitingMsgs ++ sortaTransactions })

  getBlockConsent state@TestState{..} = head (Map.keys consent ++ [Nothing])
    where
      peerCount = length testStatePeers
      threshold = div (peerCount * 2) 3
      consent = Map.filter ((>threshold) . Set.size) testStateVotesForBlock

  clearVotes state = state { testStateVotesForBlock = Map.empty }

  newRound state@TestState{..} = state { testStateRound = testStateRound + 1 }

  applyBlock state@TestState{..} (proposer, height, msgs) = state
    { testStateHeight = testStateHeight + 1
    , testStateRound = 0
    , testStateState = testStateState ++ msgs
    }

testIds :: [String]
testIds = ["n-" ++ printf "%03d" (i + 1) | i <- [0 :: Int .. 3]]

testLTMMap :: Map.Map String (LTM TestState)
testLTMMap = Map.fromList [(i, lightweightThundermint $ emptyTestState i testIds) | i <- testIds]

findSomeOuts processors = searchOut [] [] $ Map.toList processors
  where
    searchOut outputs acc [] = (outputs, Map.fromAscList $ reverse acc)
    searchOut outputs acc ((i, ltm) : iltms) = case ltm of
      O (Left o) sp -> searchOut ((i, Left o):outputs) acc ((i, sp) : iltms)
      O _        sp -> searchOut              outputs  acc ((i, sp) : iltms)
      _ -> searchOut outputs ((i,ltm) : acc) iltms

run :: [Either (LTMOut TestState) (String, [String])] -> [Either (LTMOut TestState) (String, [String])]
    -> Map.Map String (LTM TestState) -> [(String, Either (LTMOut TestState) (String, [String]))]
run [] [] processors = case findSomeOuts processors of
  ([], procs)
    | not (null stopped) -> error $ "stopped: "++show stopped
    | otherwise -> error "no messages and no outputs!"
  (os, procs) -> os ++ run [] (map snd os) procs
  where
    stopped = [ i | (i, N) <- Map.toList processors]
run acc (msg : msgs) processors = case msg of
  Right (dest, txs) -> case processor of
    I f -> let sp = f (InMessage 0 0 PreVote $ Right txs) in run acc msgs $ Map.insert dest sp processors
    O (Left o) sp -> (dest, Left o) : run acc (msg : (msgs ++ [Left o])) (Map.insert dest sp processors)
    O _ sp -> run acc (msg : msgs) (Map.insert dest sp processors)
    N -> error "stopped???"
    where
      processor = Map.findWithDefault (error "no processor in msgs") dest processors
  -- no notion of time right now.
  Left (RequestTimeout _ _ _ _) -> run acc msgs processors
  -- new height is purely bureacratic for us.
  Left (NewHeight _) -> run acc msgs processors
  Left (OutMessage h r vs om@(dest, src, _)) -> case processor of
    N -> error "definitely an internal error"
    I f -> let sp = f (InMessage h r vs $ Left om) in run acc msgs $ Map.insert dest sp processors
    O (Left o) sp -> (dest, Left o) : run acc (msg : (msgs ++ [Left o])) (Map.insert dest sp processors)
    O (Right _) sp -> run acc (msg : msgs) (Map.insert dest sp processors)
    where
      processor = Map.findWithDefault (error "no processor?") dest processors

testInputs = take 20 $ zip (cycle testIds) [["msg "++printf "%3d" i]| i <- [1 :: Int ..]]

t = forM_ (run [] (map Right testInputs) testLTMMap) $ \msg -> do
  putStr (show msg)
  hFlush stdout
  getLine

