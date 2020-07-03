{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-- |
module HSChain.Mock.Coin.Types where

import Control.Monad.Catch
import Control.DeepSeq
import Codec.Serialise      (Serialise)
import qualified Data.Aeson as JSON
import Data.Maybe
import Data.Bits
import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HM
import GHC.Generics    (Generic)

import HSChain.Types.Blockchain
import HSChain.Types.Validators
import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Block data. It's simply newtype wrapper over list of
--   transactions. Newtype is needed in order to define 'BlockData'
--   instance.
newtype BData = BData { unBData :: [Tx] }
  deriving stock    (Show,Eq,Generic)
  deriving newtype  (NFData,JSON.ToJSON,JSON.FromJSON)
  deriving anyclass (Serialise)
instance CryptoHashable BData where
  hashStep = genericHashStep "hschain-examples"

-- | Error in coin transaction processing
data CoinError
  = DepositAtWrongH
  | UnexpectedSend
  | CoinError String
  deriving stock    (Show,Generic)
  deriving anyclass (Exception,NFData,JSON.FromJSON,JSON.ToJSON)

instance BlockData BData where
  type TX              BData = Tx
  type BChError        BData = CoinError
  type Alg             BData = Ed25519 :& SHA256
  proposerSelection        = ProposerSelection randomProposerSHA256
  logBlockData (BData txs) = HM.singleton "Ntx" $ JSON.toJSON $ length txs

-- | Single transaction. We have two different transaction one to add
--   money to account ex nihilo and one to transfer money between
--   accounts.
data Tx
  = Deposit !(PublicKey (Alg BData)) !Integer
    -- ^ Deposit tokens to given key. Could only appear in genesis
    --   block (H=0)
  | Send !(PublicKey (Alg BData)) !(Signature (Alg BData)) !TxSend
    -- ^ Send coins to other addresses. Transaction must obey
    --   following invariants:
    --
    --   0. Signature must be valid
    --   1. All inputs must be owned by transaction issuer
    --   3. Inputs and outputs must be nonempty
    --   2. Sum of inputs must be equal to sum of outputs
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData, JSON.ToJSON, JSON.FromJSON)

-- | Single transaction for transfer of coins.
data TxSend = TxSend
  { txInputs  :: [UTXO]         -- ^ List of inputs that are spent in this TX
  , txOutputs :: [Unspent]      -- ^ List of outputs of this TX
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData, JSON.ToJSON, JSON.FromJSON)

-- | Pair of transaction hash and output number
data UTXO = UTXO !Int !(Hashed (Alg BData) Tx)
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData, JSON.ToJSON, JSON.FromJSON)

-- | Unspent coins belonging to some private key. It's identified by
--   public key and unspent amount.
data Unspent = Unspent !(PublicKey (Alg BData)) !Integer
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData, JSON.ToJSON, JSON.FromJSON)

instance CryptoHashable TxSend where
  hashStep = genericHashStep "hschain"
instance CryptoHashable Tx where
  hashStep = genericHashStep "hschain"
instance CryptoHashable UTXO where
  hashStep = genericHashStep "hschain"
instance CryptoHashable Unspent where
  hashStep = genericHashStep "hschain"



-- | Select proposers using PRNG based on SHA256.
randomProposerSHA256 :: Crypto alg => ValidatorSet alg -> Height -> Round -> ValidatorIdx alg
randomProposerSHA256 valSet h r
  = fromMaybe (error "randomProposerSHA256: invalid index")
  $ indexByIntervalPoint valSet
  $ fromInteger
  -- NOTE: We just compute modulo total voting power. This gives
  --       _biased_ results. But since range of SHA256 is enormous:
  --       2^256 even for voting power on order 2^64 bias will be on
  --       order 10^{-67} that is negligible
  $ (`mod` fromIntegral (totalVotingPower valSet))
  -- Convert hash to integer. We interpret hash as LE integer
  $ BS.foldr' (\w i -> (i `shiftL` 8) + fromIntegral  w) 0 bs
  where
    Hash bs = hash (valSet, h, r) :: Hash SHA256
