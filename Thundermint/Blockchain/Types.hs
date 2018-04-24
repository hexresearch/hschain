{-# LANGUAGE DataKinds #-}
-- |
-- Data types for storage of blockchain 
module Thundermint.Blockchain.Types where

import Control.Concurrent.STM
import           Data.Map (Map)

import Thundermint.Crypto
import Thundermint.Consensus.Types


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Full state of application.
data AppState alg a = AppState
  { appBlockchain    :: TVar (Blockchain alg a)
    -- ^ We store blockchain in TVar as crude approximation of
    --   in-memory database
  , appBlockStore    :: TVar (Map (BlockID alg a) (Block alg a))
    -- ^ Storage for all known blocks
    --
    --   FIXME: really??? We do need to store all blocks at current
    --          height but not all blocks
  , appProposalMaker :: Round -> Maybe (Commit alg a) -> STM (Proposal alg a)
    -- ^ FIXME: STM? Proposer must not block for long time
  , appValidator     :: PrivValidator alg a
    -- ^ Validator set is assumed to be 
  , appValidatorsSet :: Map (Address alg) (Validator alg)
    -- ^ Set of all validators including our own
    --
    --   FIXME: at the moment it's assumed to be immutable but we will
    --          need to add support of changing set as result of
    --          commited block.
  , appLogger        :: String -> IO ()
  , appMaxHeight     :: Maybe Height
  }
  
-- | Blockchain where each block stores value of type. At the moment
--   we keep whole blockchain in the memory and thing will become much
--   more complicated once we add persistence.
--
--   Note that integrity of blockchain is not maintained by
--   construction!
data Blockchain alg a
  = Genesis (Block alg a)
  -- ^ Genesis block of blockchain
  | Cons (Block alg a) (Blockchain alg a)
  -- ^ Cons cell of chain

-- | Remote validator
data Validator alg = Validator
  { validatorPubKey      :: PublicKey alg
  , validatorVotingPower :: Integer
  }

-- | Our own validator
data PrivValidator alg a = PrivValidator
  { validatorPrivKey  :: PrivKey alg
  , validateBlockData :: a -> Bool
  }

----------------------------------------------------------------
-- Messages
----------------------------------------------------------------

-- | Message received by main application
data MessageRx alg a
  = RxPreVote   (Signed 'Unverified alg (Vote 'PreVote   alg a))
  | RxPreCommit (Signed 'Unverified alg (Vote 'PreCommit alg a))
  | RxProposal  (Signed 'Unverified alg (Proposal alg a))
  | RxTimeout   Timeout
  deriving (Show)

-- | Message sent by main application
data MessageTx alg a
  = TxPreVote   (Signed 'Verified alg (Vote 'PreVote   alg a))
  | TxPreCommit (Signed 'Verified alg (Vote 'PreCommit alg a))
  | TxProposal  (Signed 'Verified alg (Proposal alg a))
  deriving (Show)
  
-- | Application connection to outer world
data AppChans alg a = AppChans
  { appChanRx          :: TChan (MessageRx alg a)
    -- ^ TChan for sending messages to the main application
  , appChanTx          :: TChan (MessageTx alg a)
    -- ^ TChan for broadcasting messages to the peers
  }

newAppChans :: IO (AppChans alg a)
newAppChans =
  AppChans <$> newTChanIO <*> newBroadcastTChanIO
