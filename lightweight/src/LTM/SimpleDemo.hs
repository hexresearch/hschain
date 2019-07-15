-- |LTM.SimpleDemo
--
-- Simplest demonstration of new functionality.
--

{-# LANGUAGE RecordWildCards, TypeFamilies, UndecidableInstances #-}

module LTM.SimpleDemo where

import Control.Monad

import qualified Data.Map as Map
import qualified Data.Set as Set

import System.IO

import Text.Printf

import LTM.LTM
import LTM.SP

--------------------------------------------------------------------------------
-- Simple demo of a 4 nodes reaching consensus.
--
-- Implements consensus demoing the core:
--   how to specify consensus
--   how to implement mempool or something alike.

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

demo n = forM_ (take n $ run [] (map Right testInputs) testLTMMap) $ \msg -> do
  putStrLn (show msg)

