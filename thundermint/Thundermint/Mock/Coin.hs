{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
-- |
-- Simple coin for experimenting with blockchain
module Thundermint.Mock.Coin (
    Alg
  , TxSend(..)
  , Tx(..)
    -- * Pure state
  , CoinState(..)
  , transitions
    -- * DB based state
  , CoinStateDB(..)
  , transitionsDB
  ) where

import Control.Applicative
import Control.Monad
import Control.DeepSeq
import Codec.Serialise      (Serialise,serialise)
import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.Map             (Map)
import qualified Data.Map.Strict  as Map
import Lens.Micro
import GHC.Generics    (Generic)

import Thundermint.Blockchain.Types
import Thundermint.Blockchain.Interpretation
import Thundermint.Crypto
import Thundermint.Crypto.Ed25519
import Thundermint.Store.SQL
import Thundermint.Store.Internal.Types


type Alg = Ed25519_SHA512

-- | Single transaction for transfer of coins
data TxSend = TxSend
  { txInputs  :: [(Hash Alg, Int)]
  , txOutputs :: [(PublicKey Alg, Integer)]
  }
  deriving (Show, Eq, Ord, Generic)
instance Serialise TxSend
instance NFData    TxSend
instance JSON.ToJSON   TxSend
instance JSON.FromJSON TxSend

data Tx
  = Deposit !(PublicKey Alg) !Integer
    -- ^ Deposit tokens to given key. Could only appear in genesis
    --   block
  | Send !(PublicKey Alg) !(Signature Alg) !TxSend
    -- ^ Send coins to other addresses. Transaction must obey
    --   following invariants:
    --
    --   0. Signature must be valid
    --   1. All inputs must be owned by transaction issuer
    --   3. Inputs and outputs must be nonempty
    --   2. Sum of inputs must be equal to sum of outputs
  deriving (Show, Eq, Ord, Generic)
instance Serialise Tx
instance NFData    Tx
instance JSON.ToJSON   Tx
instance JSON.FromJSON Tx

-- | State of coins in program-digestible format
--
--   Really we'll need to keep in DB to persist it.
newtype CoinState = CoinState
  { unspentOutputs :: Map (Hash Alg, Int) (PublicKey Alg, Integer)
    -- ^ Map of unspent outputs of transaction. It maps pair of
    --   transaction hash and output index to amount of coins stored
    --   there.
  }
  deriving (Show, NFData, Generic)

processDeposit :: Tx -> CoinState -> Maybe CoinState
processDeposit Send{}                _             = Nothing
processDeposit tx@(Deposit pk nCoin) CoinState{..} =
  return CoinState
    { unspentOutputs = Map.insert (hash tx,0) (pk,nCoin) unspentOutputs
    }


processTransaction :: Tx -> CoinState -> Maybe CoinState
processTransaction Deposit{} _ = Nothing
processTransaction transaction@(Send pubK sig txSend@TxSend{..}) CoinState{..} = do
  -- Signature must be valid
  guard $ verifyCborSignature pubK txSend sig
  -- Inputs and outputs are not null
  guard $ not $ null txInputs
  guard $ not $ null txOutputs
  -- Outputs are all positive
  forM_ txOutputs $ \(_,n) -> guard (n > 0)
  -- Inputs are owned Spend and generated amount match and transaction
  -- issuer have rights to funds
  inputs <- forM txInputs $ \i -> do
    (pk,n) <- Map.lookup i unspentOutputs
    guard $ pk == pubK
    return n
  guard (sum inputs == sum (map snd txOutputs))
  -- Update application state
  let txHash = hashBlob $ toStrict $ serialise transaction
  return CoinState
    { unspentOutputs =
        let spend txMap = foldl' (flip  Map.delete) txMap txInputs
            add   txMap = foldl'
                            (\m (i,out) -> Map.insert (txHash,i) out m)
                            txMap ([0..] `zip` txOutputs)
        in add $ spend unspentOutputs
    }

transitions :: BlockFold CoinState [Tx]
transitions = BlockFold
  { processTx           = process
  , processBlock        = \h txs s0 -> foldM (flip (process h)) s0 txs
  , transactionsToBlock = \_ ->
      let selectTx _ []     = []
          selectTx c (t:tx) = case processTransaction t c of
                                Nothing -> selectTx c  tx
                                Just c' -> t : selectTx c' tx
      in selectTx
  , initialState        = CoinState Map.empty
  }
  where
    process (Height 0) t s = processDeposit t s <|> processTransaction t s
    process _          t s = processTransaction t s


----------------------------------------------------------------
-- DB based API
----------------------------------------------------------------

newtype CoinStateDB f = CoinStateDB
  { unspentOutputsDB :: f (PMap (Hash Alg, Int) (PublicKey Alg, Integer))
  }


transitionsDB :: PersistentState CoinStateDB Alg [Tx]
transitionsDB = PersistentState
  { processTxDB           = \_ -> processTransactionDB
  , processBlockDB        = \h txs -> forM_ txs (processDB h)
  , transactionsToBlockDB = \h ->
      let selectTx []     = return []
          selectTx (t:tx) = optional (processDB h t) >>= \case
            Nothing -> return tx
            Just () -> (t :) <$> selectTx tx
      in selectTx
  -- DB schema
  , persistedData = CoinStateDB
      { unspentOutputsDB = wrap $ PMap { pmapTableName = "utxo"
                                       , pmapEncodingK = encodingCBOR
                                       , pmapEncodingV = encodingCBOR
                                       }
      }
  }



processDB
  :: (Monad (q CoinStateDB), Alternative (q CoinStateDB), ExecutorRW q)
  => Height
  -> Tx
  -> q CoinStateDB ()
processDB (Height 0) tx = processDepositDB tx <|> processTransactionDB tx
processDB _          tx = processTransactionDB tx

processDepositDB :: (Monad (q CoinStateDB), Alternative (q CoinStateDB), ExecutorRW q) => Tx -> q CoinStateDB ()
processDepositDB Send{}                = fail ""
processDepositDB tx@(Deposit pk nCoin) = do
  storeKey unspentOutputsLens (hash tx,0) (pk,nCoin)



processTransactionDB
  :: (Monad (q CoinStateDB), Alternative (q CoinStateDB), ExecutorRW q)
  => Tx
  -> q CoinStateDB ()
processTransactionDB Deposit{} = fail ""
processTransactionDB transaction@(Send pubK sig txSend@TxSend{..}) = do
  -- Signature must be valid
  guard $ verifyCborSignature pubK txSend sig
  -- Inputs and outputs are not null
  guard $ not $ null txInputs
  guard $ not $ null txOutputs
  -- Outputs are all positive
  forM_ txOutputs $ \(_,n) -> guard (n > 0)
  -- Inputs are owned Spend and generated amount match and transaction
  -- issuer have rights to funds
  inputs <- forM txInputs $ \i -> do
    Just (pk,n) <- lookupKey unspentOutputsLens i
    guard $ pk == pubK
    return n
  guard (sum inputs == sum (map snd txOutputs))
  -- Update application state
  let txHash = hashBlob $ toStrict $ serialise transaction
  forM_ txInputs  $ dropKey  unspentOutputsLens
  forM_ ([0..] `zip` txOutputs) $ \(i,out) ->
    storeKey unspentOutputsLens (txHash,i) out

unspentOutputsLens :: Lens' (CoinStateDB f) (f (PMap (Hash Alg, Int) (PublicKey Alg, Integer)))
unspentOutputsLens = lens unspentOutputsDB (const CoinStateDB)
