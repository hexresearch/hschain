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

import Control.Monad
import Control.Monad.Catch
import Control.DeepSeq
import Codec.Serialise      (Serialise)
import qualified Data.Aeson as JSON
import Data.Foldable
import Data.Maybe
import Data.Bits
import Data.Map             (Map)
import qualified Data.ByteString     as BS
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.HashMap.Strict as HM
import GHC.Generics    (Generic)

import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
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
  deriving anyclass (Exception,NFData)

instance BlockData BData where
  type TX              BData = Tx
  type BlockchainState BData = CoinState
  type BChError        BData = CoinError
  type BChMonad        BData = Either CoinError
  type Alg             BData = Ed25519 :& SHA256
  bchLogic                 = coinLogic
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

-- | State of coins in program-digestible format
data CoinState = CoinState
  { unspentOutputs :: !(Map UTXO Unspent)
    -- ^ Map of unspent outputs of transaction. It maps pair of
    --   transaction hash and output index to amount of coins stored
    --   there.
  , utxoLookup     :: !(Map (PublicKey (Alg BData)) (Set.Set UTXO))
    -- ^ UTXO set partitioned by corresponding public key. Only needed
    --   for efficient TX generation
  }
  deriving stock    (Show,   Generic)
  deriving anyclass (NFData, Serialise)

instance CryptoHashable TxSend where
  hashStep = genericHashStep "hschain"
instance CryptoHashable Tx where
  hashStep = genericHashStep "hschain"
instance CryptoHashable UTXO where
  hashStep = genericHashStep "hschain"
instance CryptoHashable Unspent where
  hashStep = genericHashStep "hschain"
instance CryptoHashable CoinState where
  hashStep = genericHashStep "hschain"



----------------------------------------------------------------
-- Basic coin logic
----------------------------------------------------------------

-- | Complete description of blockchain logic. Since we keep all state
--   in memory we simply use @Maybe@ to track failures
coinLogic :: BChLogic (Either CoinError) BData
coinLogic = BChLogic
  { processTx     = \BChEval{..} -> void $ processTransaction bchValue (merkleValue blockchainState)
  --
  , processBlock  = \BChEval{..} -> do
      let h    = blockHeight bchValue
          step = flip $ process h
      st <- foldM step (merkleValue blockchainState)
          $ let BData txs = merkleValue $ blockData bchValue
            in txs
      return BChEval { bchValue        = ()
                     , blockchainState = merkled st
                     , ..
                     }
  --
  , generateBlock = \NewBlock{..} txs -> do
      let selectTx c []     = (c,[])
          selectTx c (t:tx) = case processTransaction t c of
                                Left  _  -> selectTx c  tx
                                Right c' -> let (c'', b  ) = selectTx c' tx
                                            in  (c'', t:b)
      let (st', dat) = selectTx (merkleValue newBlockState) txs
      return BChEval { bchValue        = BData dat
                     , validatorSet    = merkled newBlockValSet
                     , blockchainState = merkled st'
                     }
  }
  where
    process (Height 0) t s = case processDeposit t s of
      Right x -> Right x
      Left  _ -> processTransaction t s
    process _          t s = processTransaction t s


-- | Process deposit transaction. Should only be called for
--   transactions from genesis block
processDeposit :: Tx -> CoinState -> Either CoinError CoinState
processDeposit Send{}                _             = Left UnexpectedSend
processDeposit tx@(Deposit pk nCoin) CoinState{..} =
  return CoinState
    { unspentOutputs = Map.insert utxo (Unspent pk nCoin) unspentOutputs
    , utxoLookup     = Map.alter (\case
                                     Nothing -> Just (Set.singleton utxo)
                                     Just s  -> Just (Set.insert utxo s)
                                 ) pk utxoLookup
    }
  where
    utxo = UTXO 0 (hashed tx)

-- | Process money movement transaction
processTransaction :: Tx -> CoinState -> Either CoinError CoinState
processTransaction Deposit{} _ = Left DepositAtWrongH
processTransaction transaction@(Send pubK sig txSend@TxSend{..}) CoinState{..} = do
  -- Inputs and outputs are not null
  when (null txInputs)  $ Left $ CoinError "Empty input  list"
  when (null txOutputs) $ Left $ CoinError "Empty output list"
  -- Outputs are all positive
  forM_ txOutputs $ \(Unspent _ n) ->
    unless (n > 0) $ Left $ CoinError "Negative output"
  -- Inputs are owned Spend and generated amount match and transaction
  -- issuer have rights to funds
  inputs <- forM txInputs $ \i -> do
    Unspent pk n <- case Map.lookup i unspentOutputs of
      Just x  -> return x
      Nothing -> Left $ CoinError "Unknown input"
    unless (pk == pubK) $ Left $ CoinError "Mismatch of publick keys"
    return n
  unless (sum inputs == sum [n | Unspent _ n <- txOutputs])
    $ Left $ CoinError "Missmatch between inputs and outputs"
  -- Signature must be valid. Note signature check is expensive so
  -- it's done at last moment
  unless (verifySignatureHashed pubK txSend sig)
    $ Left $ CoinError "Invalid signature"
  -- Update application state
  let txHash  = hashed transaction
  return CoinState
    { unspentOutputs =
        let spend txMap = foldl' (flip  Map.delete) txMap txInputs
            add   txMap = foldl'
                            (\m (i,out) -> Map.insert (UTXO i txHash) out m)
                            txMap ([0..] `zip` txOutputs)
        in add $ spend unspentOutputs
    , utxoLookup =
        let insert m (i,Unspent k _) = Map.alter
              (\case
                  Nothing -> Just $ Set.singleton (UTXO i txHash)
                  Just s  -> Just $ Set.insert    (UTXO i txHash) s
              ) k m
            add    utxos = foldl' insert utxos ([0..] `zip` txOutputs)
            remove utxos = foldl' (flip Set.delete) utxos txInputs
        in add $ Map.adjust remove pubK utxoLookup
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
