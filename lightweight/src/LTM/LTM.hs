-- |Implementing thundermint as parsing over
-- stream of events, hence "Lightweight ThunderMint".
--

{-# LANGUAGE DeriveAnyClass, DeriveFunctor, GADTs #-}
{-# LANGUAGE RecordWildCards, StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module LTM.LTM where

import Control.Monad (when)

import LTM.SP

type Height = Int
type Round = Int
type Microseconds = Int

data Vote = VoteFor | VoteAgainst deriving (Eq, Ord, Show)

data VotingStage = PreVote | PreCommit deriving (Eq, Ord, Show)

data Timed msg = Timeout Height Round VotingStage | Message msg deriving (Eq, Ord, Show)

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
  proposal :: state -> Maybe (Block state, state)

  -- |Get a list of peer identifiers from state.
  peersList :: state -> [PeerId state]

  -- |Timeout for propose, microseconds.
  proposeTime :: state -> Microseconds

  -- |Receive a vote. A processor that
  -- awaits for input messages and returns
  -- voting messages - pairs (peer ID, Vote).
  -- State parameter is used as witness.
  receiveVote :: state -> SP (InputMessage state) (PeerId state, Vote)

  -- 

type LTMSP state a = SPB (Timed (InputMessage state)) (UpDown state (OutputMessage state)) a
lightweightThundermint :: LTMState state
                       => state
                       -> LTMSP state ()
lightweightThundermint state = loop state
  where
    loop state = do
      (state, block) <- proposeReceive state
      block <- precommit state block
      state <- commit state block
      loop state

proposeReceive :: LTMState state => state -> LTMSP state (state, Block state)
proposeReceive state = error "propo rec"

precommit = error "pcm"
commit = error "comt"

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

data TestState = TestState
  { testStateHeight      :: Height
  , testStateRound       :: Round
  , testStateId          :: String
  , testStatePeers       :: [String]
  , testStateState       :: [String] -- strings received.
  , testStateWaitingMsgs :: [String] -- strings waiting to be sent.
  } deriving Show

instance LTMState TestState where
  type PeerId TestState = String
  type OutputMessage TestState = ()
  type InputMessage TestState = ()
  type Block TestState = (PeerId TestState, Height, [String])

  proposal state@TestState{..}
    | mod (testStateHeight + testStateRound) n == ourIndex =
      Just
        ( (testStateId, testStateHeight, take 1 testStateWaitingMsgs)
        , state { testStateWaitingMsgs = drop 1 testStateWaitingMsgs })
    | otherwise = Nothing
    where
      n = length testStatePeers
      ourIndex = length $ takeWhile (/= testStateId) testStatePeers

  peersList state@TestState{..} = testStatePeers

  proposeTime _state = 100000

  receiveVote = undefined
