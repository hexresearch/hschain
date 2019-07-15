-- |Implementing thundermint as parsing over
-- stream of events, hence "Lightweight ThunderMint".
--

{-# LANGUAGE DeriveAnyClass, DeriveFunctor, FlexibleContexts, GADTs #-}
{-# LANGUAGE RecordWildCards, StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module LTM.LTM where

import Control.Applicative

import LTM.SP

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
                in out (NewHeight state') >>^ returnR (clearVotes state')
  Nothing -> returnR (clearVotes $ newRound state)

