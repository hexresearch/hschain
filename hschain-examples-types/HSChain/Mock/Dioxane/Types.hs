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
import Control.Applicative
import Control.Monad
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
import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- Basic coin logic
----------------------------------------------------------------

type DioAlg = Ed25519 :& SHA512

newtype BData tag = BData [Tx]
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
  deriving anyclass (Exception,NFData)

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
  type TX              (BData tag) = Tx
  type BlockchainState (BData tag) = DioState
  type BChError        (BData tag) = DioError
  type BChMonad        (BData tag) = Maybe
  type Alg             (BData tag) = DioAlg
  bchLogic                 = dioLogic
  proposerSelection        = ProposerSelection randomProposerSHA512
  logBlockData (BData txs) = HM.singleton "Ntx" $ JSON.toJSON $ length txs


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

dioLogic :: forall tag. Dio tag => BChLogic Maybe (BData tag)
dioLogic = BChLogic
  { processTx     = const empty
  --
  , processBlock  = \BChEval{..} -> do
      let sigCheck = guard
                   $ and
                   $ parMap rseq
                     (\(Tx sig tx) -> verifySignatureHashed (txFrom tx) tx sig)
                     (let BData txs = merkleValue $ blockData bchValue
                      in txs
                     )
      let update   = foldM (flip process) (merkleValue blockchainState)
                   $ (let BData txs = merkleValue $ blockData bchValue
                      in txs
                     )
      st <- uncurry (>>)
          $ withStrategy (evalTuple2 rpar rpar)
          $ (sigCheck, update)
      return BChEval { bchValue        = ()
                     , blockchainState = merkled st
                     , ..
                     }
  -- We generate one transaction for every key. And since we move
  -- money from one account to another it's quite simple to update state
  , generateBlock = \NewBlock{..} _ -> do
      let nonce = let Height h = newBlockHeight in fromIntegral h - 1
          keys  = dioUserKeys
      return $! BChEval
        { bchValue = BData
                   $ parMap rseq
                     (\(sk,pk) -> let body = TxBody
                                        { txTo     = pk
                                        , txFrom   = pk
                                        , txNonce  = nonce
                                        , txAmount = 1
                                        }
                                  in Tx { txSig  = signHashed sk body
                                        , txBody = body
                                        }
                     )
                     (V.toList keys)
        , validatorSet    = merkled newBlockValSet
        , blockchainState = merkled
                          $ userMap . each . userNonce %~ succ
                          $ merkleValue newBlockState
        }
  }
  where
    DioDict{..} = dioDict @tag


process :: Tx -> DioState -> Maybe DioState
process Tx{txBody=TxBody{..}} st = do
  ufrom  <- st ^. userMap . at txFrom
  _      <- st ^. userMap . at txTo
  -- Nonce is correct & and we have funds
  guard $ txNonce == ufrom^.userNonce
  guard $ ufrom^.userBalance >= txAmount
  return
    $! st
    & userMap . at txFrom . _Just %~ ( (userNonce   %~ succ)
                                     . (userBalance %~ subtract txAmount)
                                     )
    & userMap . at txTo   . _Just . userBalance %~ (+ txAmount)

-- | Select proposers using PRNG based on SHA512.
randomProposerSHA512 :: Crypto alg => ValidatorSet alg -> Height -> Round -> ValidatorIdx alg
randomProposerSHA512 valSet h r
  = fromMaybe (error "randomProposerSHA512: invalid index")
  $ indexByIntervalPoint valSet
  $ fromInteger
  -- NOTE: We just compute modulo total voting power. This gives
  --       _biased_ results. But since range of SHA512 is enormous:
  --       2^512 even for voting power on order 2^64 bias will be on
  --       order 10^{-134} that is negligible
  $ (`mod` fromIntegral (totalVotingPower valSet))
  -- Convert hash to integer. We interpret hash as LE integer
  $ BS.foldr' (\w i -> (i `shiftL` 8) + fromIntegral  w) 0 bs
  where
    Hash bs = hash (valSet, h, r) :: Hash SHA512
