-- |Implementing thundermint as parsing over
-- stream of events, hence "Lightweight ThunderMint".
--

{-# LANGUAGE DeriveAnyClass, DeriveFunctor, GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

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

  -- |Should we propose at this height and round?
  shouldPropose :: Height -> Round -> state -> Bool

  -- |Form the block from state, modifying state in the process.
  proposal :: Height -> Round -> state -> (block, state)

  -- |Get a list of peer identifiers from state.
  peersList :: state -> [PeerId state]

  -- |Timeout for round, microseconds.
  roundTimeout :: state -> Microseconds

  -- |Receive a vote. A processor that
  -- awaits for input messages and returns
  -- voting messages - pairs (peer ID, Vote).
  -- State parameter is used as witness.
  receiveVote :: state -> SP (InputMessage state) (PeerId state, Vote)

  -- 

lightweightThundermint :: Height -> state -> SP () ()
lightweightThundermint height state = loop height 0 state
  where
    loop height round state = do
      (state, block) <- proposeReceive height round state
      block <- precommit height round state block
      (height, round, state) <- commit height round state block
      loop height round state

-- |Send proposa
proposeReceive :: Height -> Round -> state
               -> SP (UpDown (Timed (InputMessage state)) ()) (UpDown ()())
proposeReceive height round state = do
  state' <- if shouldPropose height round state
    then do
      let (block, state') = proposal height round state
      multicast height round block state'
      return state'
    else return state
  withTimeout roundTimeout state' PreVote $ do
    scanSP selectProposal state' (receiveVote state')

selectProposal :: a
selectProposal = undefined

multicast :: Height -> Round -> Block state -> state -> SP a (OutputMessage state)
multicast height round block state = error "multicast!"

withTimeout :: (state -> Microseconds)
            -> state
            -> VotingStage
            -> SP (InputMessage state) state
            -> SP (UpDown () ()) ()
withTimeout us stage belowProcessor =
  undefined
