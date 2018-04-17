-- |
-- Data types for storage of blockchain 
module Thundermint.Blockchain.Types where

import Thundermint.Consensus.Types

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

data Validator alg = Validator
  { validatorPubKey      :: PublicKey alg
  , validatorVotingPower :: Int64
  }

data PrivValidator alg a = PrivValidator
  { validatorPrivKey  :: PrivKey alg
  , validateBlockData :: a -> Bool
  }
