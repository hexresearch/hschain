-- |LTM.SimpleDemo
--
-- Simplest demonstration of new functionality.
--

{-# LANGUAGE RecordWildCards, TypeFamilies, UndecidableInstances #-}

module LTM.KeyValueDemo where

import qualified Control.Concurrent as CC
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar

import Control.Exception (catch, SomeException)

import Control.Monad

import qualified Data.ByteString as BS

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import System.IO

import Text.Printf

import LTM.LTM
import LTM.SP

import qualified Debug.Trace as DT

--------------------------------------------------------------------------------
-- Testing key-value state with keys and values being Strings.
--

type TX = (BS.ByteString, Maybe BS.ByteString)

data KVMsg =
    KVProposal  (Block KVState)
  | KVPrevote   (Maybe (Block KVState))
  | KVPrecommit (Maybe (Block KVState))
  deriving (Eq, Ord, Show)

data KVState = KVState
  { kvStateHeight         :: !Height
  , kvStateRound          :: !Round
  , kvStateId             :: !String
  , kvStatePeers          :: [String]
  , kvStateState          :: !(Map.Map BS.ByteString BS.ByteString)
  , kvStateWaitingTxs     :: [TX] -- key and value; Nothing means deletion
  , kvStateVotesForBlock  :: !(Map.Map (Maybe (Block KVState)) (Maybe (Block KVState), Set.Set (PeerId KVState)))
  , kvStateNumTxsPerBlock :: !Int
  } deriving Show

emptyKVState :: Int -> String -> [String] -> KVState
emptyKVState numTxs id allIds = KVState
  { kvStateHeight         = 0
  , kvStateRound          = 0
  , kvStateId             = id
  , kvStatePeers          = allIds
  , kvStateState          = Map.empty
  , kvStateWaitingTxs     = []
  , kvStateVotesForBlock  = Map.empty
  , kvStateNumTxsPerBlock = numTxs
  }

onlyHeader :: Block KVState -> Block KVState
onlyHeader (pid, h, txs) = (pid, h, [])

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
      ntxs = kvStateNumTxsPerBlock
      n = length kvStatePeers
      ourIndex = length $ takeWhile (/= kvStateId) kvStatePeers
      block = (kvStateId, kvStateHeight, take ntxs kvStateWaitingTxs)

  precommitMessages state@KVState{..} =
    [(them, kvStateId, KVPrecommit maybeBlock) | them <- kvStatePeers, them /= kvStateId]
    where
      maybeBlock = getBlockConsent state

  peersList state@KVState{..} = kvStatePeers

  halfRoundTime _state = 100000

  enoughVotes KVState{..} = sum (map (Set.size . snd) $ Map.elems kvStateVotesForBlock) > threshold
    where
      peerCount = length kvStatePeers
      threshold = div (peerCount * 2) 3

  addSelfVote maybeBlock state@KVState{..} =
    state { kvStateVotesForBlock = Map.insertWith (\(mb, ss) (mb', ss') -> (mb, Set.union ss ss'))
                                         (fmap onlyHeader maybeBlock) (maybeBlock, Set.singleton kvStateId) kvStateVotesForBlock }

  {-# INLINE addVote #-}
  addVote (Left (us, them, KVProposal block)) state@KVState{..}
    = {-# SCC addVoteProposal #-} ([(them, us, KVPrevote (Just block)) | them <- kvStatePeers, them /= us]
      , state { kvStateVotesForBlock =
                   Map.insertWith (\(mb, ss) (mb', ss') -> (mb, Set.union ss ss'))
                       (Just $ onlyHeader block) (Just block, Set.singleton them) kvStateVotesForBlock }
      )
  addVote (Left (us, them, KVPrevote maybeBlock)) state@KVState{..}
    = {-# SCC addVotePrevote #-} ([], state
             { kvStateVotesForBlock =
                   Map.insertWith (\(mb, ss) (mb', ss') -> (mb, Set.union ss ss'))
                      (fmap onlyHeader maybeBlock) (maybeBlock, Set.singleton them) kvStateVotesForBlock })
  addVote (Left (us, them, KVPrecommit maybeBlock)) state@KVState{..}
    = {-# SCC addVotePrecommit #-} ([], state
             { kvStateVotesForBlock =
                   Map.insertWith (\(mb, ss) (mb', ss') -> (mb, Set.union ss ss'))
                      (fmap onlyHeader maybeBlock) (maybeBlock, Set.singleton them) kvStateVotesForBlock
             })
  addVote (Right sortaTransactions) state@KVState{..}
    = {-# SCC addVoteRight #-} ([], state { kvStateWaitingTxs = kvStateWaitingTxs ++ sortaTransactions })

  getBlockConsent state@KVState{..} = head $ map fst (Map.elems consent) ++ [Nothing]
    where
      peerCount = length kvStatePeers
      threshold = div (peerCount * 2) 3
      consent = Map.filter ((>threshold) . Set.size . snd) kvStateVotesForBlock

  clearVotes state = state { kvStateVotesForBlock = Map.empty }

  newRound state@KVState{..} = state { kvStateRound = kvStateRound + 1 }

  {-# INLINE applyBlock #-}
  applyBlock state@KVState{..} (proposer, height, msgs) =
    state
    { kvStateHeight = kvStateHeight + 1
    , kvStateRound = 0
    , kvStateState = foldl (\st (k,v) -> Map.alter (const v) k st) kvStateState msgs
    }

testIds :: [String]
testIds = ["n-" ++ printf "%03d" (i + 1) | i <- [0 :: Int .. 3]]

testLTMMap :: Int -> Map.Map String (LTM KVState)
testLTMMap ntxs = Map.fromList [(i, lightweightThundermint $ emptyKVState ntxs i testIds) | i <- testIds]

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

testInputs :: Int -> Int -> [(String, [TX])]
testInputs totalKeys numInserts =
  Map.toList $ fmap reverse $ Map.fromListWith (++) $
  zip (cycle testIds) $ map (\x -> [x]) $ add++del
  where
    keys = take numInserts $ cycle [BS.pack $ map (fromIntegral . fromEnum) $ "msg "++printf "%3d" i| i <- cycle [1..totalKeys]]
    add = zip keys (cycle (map (Just . BS.pack . map (fromIntegral . fromEnum) . ("key" ++) . show) [1..totalKeys]))
    del = zip keys (repeat Nothing)

demo n m = forM_ (take n $ run [] (map Right $ testInputs m m) $ testLTMMap 5) $ \msg -> case msg of
  (_, Left (NewHeight _)) -> putStrLn (show msg)
  _ -> return ()

performance :: Int -> Int -> Int -> IO ()
performance numTxsPerBlock totalKeys numInserts = do
  let outs = run [] (map Right $ testInputs totalKeys numInserts) $ testLTMMap numTxsPerBlock
  go 0 outs
  where
    go n ((_, Left (NewHeight KVState{..})) : msgs)
      | kvStateId == head testIds && Map.null kvStateState && n > 0 = putStrLn "done"
      | kvStateId == head testIds && not (Map.null kvStateState) && n == 0 = go 1 msgs
    go n (_:msgs) = go n msgs
    go _ [] = error "end of messages?"

threadInterpret :: String
                -> LTM KVState
                -> Map.Map (PeerId KVState) (Chan.Chan (LTMIn KVState))
                -> Chan.Chan (LTMIn KVState)
                -> Chan.Chan KVState
                -> MVar.MVar Bool
                -> IO ()
threadInterpret ourId ltm otherChans inputMessagesChan statesChan stopVar = loop False ltm
  where
    loop beenNonEmpty N = return ()
    loop beenNonEmpty (I f) = do
      i <- Chan.readChan inputMessagesChan
      loop beenNonEmpty (f i)
    loop beenNonEmpty (O (Left (NewHeight st)) p) = do
      --Chan.writeChan statesChan st
      let currentEmpty = Map.null (kvStateState st)
      when (currentEmpty && beenNonEmpty) $ MVar.putMVar stopVar True
      loop (not currentEmpty || beenNonEmpty) p
    loop beenNonEmpty (O (Left (OutMessage h r vs msg@(them, _, _))) p) = do
      Chan.writeChan (Map.findWithDefault (error "no chan???") them otherChans) (InMessage h r vs $ Left msg)
      loop beenNonEmpty p
    loop beenNonEmpty (O (Left _) p) = loop beenNonEmpty p

parallelRun :: Int -> Int -> Int -> IO (Chan.Chan KVState, MVar.MVar Bool)
parallelRun numTxsPerBlock totalKeys numInserts = do
  let inputs = testInputs totalKeys numInserts
  stop <- MVar.newMVar False
  statesChan <- Chan.newChan
  chansProcessors <- liftM Map.fromList $
    forM (Map.toList $ testLTMMap numTxsPerBlock) $ \(id, processor) -> do
      chan <- Chan.newChan
      forM_ (filter ((==id) . fst) inputs) $
        \txs -> do
          Chan.writeChan chan (InMessage 0 0 PreVote $ Right $ snd txs)
      return (id, (chan, processor))
  let chans = Map.map fst chansProcessors
  forM_ (Map.toList chansProcessors) $
    \(id, (ch, p)) ->
      CC.forkIO $ threadInterpret id p chans ch statesChan stop
  return (statesChan, stop)

eReturn :: SomeException -> IO ()
eReturn _ = return ()

parallelPerformance :: Int -> Int -> Int -> IO ()
parallelPerformance numTxsPerBlock totalKeys numInserts = do
  (statesChan, stopVar) <- parallelRun numTxsPerBlock totalKeys numInserts
  go stopVar
  where
    go stopVar = do
      MVar.takeMVar stopVar
      return ()
