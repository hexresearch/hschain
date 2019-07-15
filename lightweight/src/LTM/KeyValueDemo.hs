-- |LTM.SimpleDemo
--
-- Simplest demonstration of new functionality.
--

{-# LANGUAGE RecordWildCards, TypeFamilies, UndecidableInstances #-}

module LTM.KeyValueDemo where

import Control.Monad

import qualified Data.Map as Map
import qualified Data.Set as Set

import System.IO

import Text.Printf

import LTM.LTM
import LTM.SP

--------------------------------------------------------------------------------
-- Testing key-value state with keys and values being Strings.
--

type TX = (String, Maybe String)

data KVMsg =
    KVProposal  (Block KVState)
  | KVPrevote   (Maybe (Block KVState))
  | KVPrecommit (Maybe (Block KVState))
  deriving (Eq, Ord, Show)

data KVState = KVState
  { kvStateHeight        :: Height
  , kvStateRound         :: Round
  , kvStateId            :: String
  , kvStatePeers         :: [String]
  , kvStateState         :: Map.Map String String
  , kvStateWaitingTxs    :: [TX] -- key and value; Nothing means deletion
  , kvStateVotesForBlock :: Map.Map (Maybe (Block KVState)) (Set.Set (PeerId KVState))
  } deriving Show

emptyKVState :: String -> [String] -> KVState
emptyKVState id allIds = KVState
  { kvStateHeight        = 0
  , kvStateRound         = 0
  , kvStateId            = id
  , kvStatePeers         = allIds
  , kvStateState         = Map.empty
  , kvStateWaitingTxs    = []
  , kvStateVotesForBlock = Map.empty
  }

instance LTMState KVState where
  type PeerId KVState = String
  type OutputMessage KVState = (PeerId KVState, PeerId KVState, KVMsg)
  type InputMessage KVState = Either (OutputMessage KVState) [TX]
  type Block KVState = (PeerId KVState, Height, [TX])

  getHeightRound state@KVState{..} = (kvStateHeight, kvStateRound)

  proposal state@KVState{..}
    | mod (kvStateHeight + kvStateRound) n == ourIndex =
      Just
        ( block
        , [(dest, kvStateId, KVProposal block) | dest <- kvStatePeers, dest /= kvStateId]
        , state { kvStateWaitingTxs = drop ntxs kvStateWaitingTxs })
    | otherwise = Nothing
    where
      ntxs = 5
      n = length kvStatePeers
      ourIndex = length $ takeWhile (/= kvStateId) kvStatePeers
      block = (kvStateId, kvStateHeight, take ntxs kvStateWaitingTxs)

  precommitMessages state@KVState{..} =
    [(them, kvStateId, KVPrecommit maybeBlock) | them <- kvStatePeers, them /= kvStateId]
    where
      maybeBlock = getBlockConsent state

  peersList state@KVState{..} = kvStatePeers

  halfRoundTime _state = 100000

  enoughVotes KVState{..} = sum (map Set.size $ Map.elems kvStateVotesForBlock) > threshold
    where
      peerCount = length kvStatePeers
      threshold = div (peerCount * 2) 3

  addSelfVote maybeBlock state@KVState{..} =
    state { kvStateVotesForBlock = Map.insertWith Set.union maybeBlock (Set.singleton kvStateId) kvStateVotesForBlock }

  addVote (Left (us, them, KVProposal block)) state@KVState {..}
    = ([(them, us, KVPrevote (Just block)) | them <- kvStatePeers, them /= us]
      , state { kvStateVotesForBlock = Map.insertWith Set.union (Just block) (Set.singleton them) kvStateVotesForBlock }
      )
  addVote (Left (us, them, KVPrevote maybeBlock)) state@KVState {..}
    = ([], state { kvStateVotesForBlock = Map.insertWith Set.union maybeBlock (Set.singleton them) kvStateVotesForBlock })
  addVote (Left (us, them, KVPrecommit maybeBlock)) state@KVState {..}
    = ([], state { kvStateVotesForBlock = Map.insertWith Set.union maybeBlock (Set.singleton them) kvStateVotesForBlock })
  addVote (Right sortaTransactions) state@KVState {..}
    = ([], state { kvStateWaitingTxs = kvStateWaitingTxs ++ sortaTransactions })

  getBlockConsent state@KVState{..} = head (Map.keys consent ++ [Nothing])
    where
      peerCount = length kvStatePeers
      threshold = div (peerCount * 2) 3
      consent = Map.filter ((>threshold) . Set.size) kvStateVotesForBlock

  clearVotes state = state { kvStateVotesForBlock = Map.empty }

  newRound state@KVState{..} = state { kvStateRound = kvStateRound + 1 }

  applyBlock state@KVState{..} (proposer, height, msgs) = state
    { kvStateHeight = kvStateHeight + 1
    , kvStateRound = 0
    , kvStateState = foldr (\(k,v) st -> Map.alter (const v) k st) kvStateState msgs
    }

testIds :: [String]
testIds = ["n-" ++ printf "%03d" (i + 1) | i <- [0 :: Int .. 3]]

testLTMMap :: Map.Map String (LTM KVState)
testLTMMap = Map.fromList [(i, lightweightThundermint $ emptyKVState i testIds) | i <- testIds]

findSomeOuts processors = searchOut [] [] $ Map.toList processors
  where
    searchOut outputs acc [] = (outputs, Map.fromAscList $ reverse acc)
    searchOut outputs acc ((i, ltm) : iltms) = case ltm of
      O (Left o) sp -> searchOut ((i, Left o):outputs) acc ((i, sp) : iltms)
      O _        sp -> searchOut              outputs  acc ((i, sp) : iltms)
      _ -> searchOut outputs ((i,ltm) : acc) iltms

run :: [Either (LTMOut KVState) (String, [TX])] -> [Either (LTMOut KVState) (String, [TX])]
    -> Map.Map String (LTM KVState) -> [(String, Either (LTMOut KVState) (String, [TX]))]
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

testInputs :: [(String, [TX])]
testInputs = zip (cycle testIds) $ map (\x -> [x]) $ add++del
  where
    n = 20
    keys = take n ["msg "++printf "%3d" i| i <- cycle [1 :: Int .. 7]]
    add = zip keys (cycle (map (Just . show) [1..5]))
    del = zip keys (repeat Nothing)

demo n = forM_ (take n $ run [] (map Right testInputs) testLTMMap) $ \msg -> case msg of
  (_, Left (NewHeight _)) -> putStrLn (show msg)
  _ -> return ()

