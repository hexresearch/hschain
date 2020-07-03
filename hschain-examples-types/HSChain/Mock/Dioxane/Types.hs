{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-- |
module HSChain.Mock.Dioxane.Types where

import Codec.Serialise      (Serialise)
import Control.Monad.Catch
import Control.Parallel.Strategies
import Data.Int
import Data.Bits
import Data.Maybe
import qualified Data.Aeson          as JSON
import qualified Data.Map.Strict     as Map
import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString     as BS
import Control.Lens
import GHC.Generics (Generic)

import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.Types


----------------------------------------------------------------
-- Basic coin logic
----------------------------------------------------------------

type DioAlg = Ed25519 :& SHA256

newtype BData tag = BData { unBData :: [Tx] }
  deriving stock    (Show,Eq,Generic)
  deriving newtype  (NFData,JSON.ToJSON,JSON.FromJSON)
  deriving anyclass (Serialise)
instance CryptoHashable (BData tag) where
  hashStep = genericHashStep "hschain-examples"

data Tx = Tx
  { txSig  :: !(Signature DioAlg)
  , txBody :: !TxBody
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData, JSON.ToJSON, JSON.FromJSON)

data TxBody = TxBody
  { txFrom   :: !(PublicKey DioAlg)
  , txTo     :: !(PublicKey DioAlg)
  , txNonce  :: !Int64
  , txAmount :: !Int64
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData, JSON.ToJSON, JSON.FromJSON)

data DioError = DioError
  deriving stock    (Show,Generic)
  deriving anyclass (Exception,NFData,JSON.FromJSON,JSON.ToJSON)

data DioState = DioState
  { _userMap :: Map.Map (PublicKey DioAlg) UserState
  }
  deriving stock    (Show,   Generic)
  deriving anyclass (NFData, Serialise)

data UserState = UserState
  { _userNonce   :: !Int64
  , _userBalance :: !Int64
  }
  deriving stock    (Show,   Generic)
  deriving anyclass (NFData, Serialise)

instance CryptoHashable Tx where
  hashStep = genericHashStep "hschain.dioxane"
instance CryptoHashable TxBody where
  hashStep = genericHashStep "hschain.dioxane"
instance CryptoHashable DioState where
  hashStep = genericHashStep "hschain.dioxane"
instance CryptoHashable UserState where
  hashStep = genericHashStep "hschain.dioxane"


makeLenses ''UserState
makeLenses ''DioState

instance Dio tag => BlockData (BData tag) where
  type TX       (BData tag) = Tx
  type BChError (BData tag) = DioError
  type Alg      (BData tag) = DioAlg
  proposerSelection         = ProposerSelection randomProposerSHA256
  logBlockData (BData txs)  = HM.singleton "Ntx" $ JSON.toJSON $ length txs


----------------------------------------------------------------
-- Logic
----------------------------------------------------------------

class Dio a where
  dioDict :: DioDict a

data DioDict a = DioDict
  { dioUserKeys       :: V.Vector ( PrivKey   DioAlg
                                  , PublicKey DioAlg
                                  )
  , dioInitialBalance :: Int64
  , dioValidators     :: Int64
  }

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
