{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-- |
module HSChain.Examples.Coin where

import Control.Concurrent (killThread, threadDelay)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.DeepSeq
import Codec.Serialise      (Serialise)
import qualified Data.Aeson as JSON
import Data.Maybe
import Data.Word
import qualified Database.SQLite.Simple.ToField   as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import GHC.Generics    (Generic)

import HSChain.Types.Merkle.Types
import HSChain.Control.Class
import HSChain.Control.Channels
import HSChain.Control.Util
import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.PoW.P2P
import HSChain.Store.Query


----------------------------------------------------------------
-- Blovckchain block
----------------------------------------------------------------

data Coin f = Coin
  { coinData   :: !(MerkleNode f Alg [Tx Coin])
    -- ^ List of transactions. First one is coinbase transactions.
  , coinTarget :: !Target
    -- ^ Current difficulty of mining. It means a complicated thing
    --   right now.
  , coinNonce  :: !Word64
    -- ^ Nonce which proves PoW puzzle
  }
  deriving (Generic)

deriving anyclass instance Serialise (Coin Identity)
deriving anyclass instance Serialise (Coin Proxy)
deriving stock    instance Show      (Coin Identity)
deriving stock    instance Show      (Coin Proxy)
deriving stock    instance Eq        (Coin Identity)
deriving stock    instance Eq        (Coin Proxy)


instance BlockData Coin where
  newtype BlockID Coin = CoinID (Hash SHA256)
    deriving newtype ( Show,Eq,Ord,CryptoHashable,Serialise,ByteRepr
                     , JSON.ToJSON, JSON.FromJSON)
    deriving (SQL.FromField, SQL.ToField) via ByteRepred (BlockID Coin)

  newtype TxID Coin = CoinTxID (Hash SHA256)
    deriving newtype ( Show,Eq,Ord,CryptoHashable,Serialise,ByteRepr
                     , JSON.ToJSON, JSON.FromJSON)
    deriving (SQL.FromField, SQL.ToField) via ByteRepred (BlockID Coin)

  data BlockException Coin = CoinError String
    deriving stock    (Show)
    deriving anyclass (Exception)

  type Tx Coin = TxCoin

  blockID = CoinID   . hash
  txID    = CoinTxID . hash

  validateTxContextFree = undefined

  validateHeader bh (Time now) header
    | blockHeight header == 0 = return True
    | otherwise               = return $ and
      [ let Time t = blockTime header
        in t <= now + (2*60*60*1000)
      , coinTarget (blockData header) == retarget bh
      , blockTargetThreshold header >= hash256AsTarget header
      ]
  -- At he moment we accept only empty blocks (for simplicity)
  validateBlock b = return $ null $ merkleValue $ coinData $ blockData b
  blockWork b = Work
              $ fromIntegral  $ ((2^(256 :: Int)) `div`)
              $ targetInteger $ coinTarget $ blockData b
  blockTargetThreshold b = Target $ targetInteger (coinTarget (blockData b))
  targetAdjustmentInfo _ = let n = 100 in (Height n, timeSecond)



instance Mineable Coin where
  adjustPuzzle b0 = return
    ( listToMaybe
      [ b
      | nonce <- [minBound .. maxBound]
      , let b    = b0 { blockData = (blockData b0) { coinNonce = nonce } }
            tgt' = hash256AsTarget b
      , tgt' <= tgt
      ]
    , Target 0
    )
    where
      tgt = blockTargetThreshold b0

instance MerkleMap Coin where
  merkleMap f c = c { coinData = mapMerkleNode f (coinData c) }

instance (IsMerkle f) => CryptoHashable (Coin f) where
  hashStep = genericHashStep "hschain"



----------------------------------------------------------------
-- Transactions
----------------------------------------------------------------


-- | Crypto algorithms used by mock coin
type Alg  = Ed25519 :& SHA256

-- | Single transaction. It's just transfer of fund between different
--   accounts, Here we use very simple UTXO model where outputs are
--   simply protected by signatures.
--
--   Token transfer is valid if following conditions are satistified:
--
--   0. Signature must be valid
--   1. All inputs must be owned by transaction issuer
--   3. Inputs and outputs must be nonempty
--   2. Sum of inputs must be equal to sum of outputs
data TxCoin = TxCoin !(PublicKey Alg) !(Signature Alg) !TxSend
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData, JSON.ToJSON, JSON.FromJSON)
  deriving CryptoHashable via CryptoHashablePackage "hschain" TxCoin

-- | Single transaction for transfer of coins.
data TxSend = TxSend
  { txInputs  :: [UTXO]         -- ^ List of inputs that are spent in this TX
  , txOutputs :: [Unspent]      -- ^ List of outputs of this TX
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData, JSON.ToJSON, JSON.FromJSON)
  deriving CryptoHashable via CryptoHashablePackage "hschain" TxSend

-- | Pair of transaction hash and output number
data UTXO = UTXO !Int !(Hashed Alg TxCoin)
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData, JSON.ToJSON, JSON.FromJSON)
  deriving CryptoHashable via CryptoHashablePackage "hschain" UTXO

-- | Unspent coins belonging to some private key. It's identified by
--   public key and unspent amount.
data Unspent = Unspent !(PublicKey Alg) !Integer
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData, JSON.ToJSON, JSON.FromJSON)
  deriving CryptoHashable via CryptoHashablePackage "hschain" Unspent


----------------------------------------------------------------
-- Mining loop
----------------------------------------------------------------

miningLoop :: MonadFork m => PoW s m Coin -> Bool -> m x
miningLoop _   False = liftIO $ forever $ threadDelay maxBound
miningLoop pow True  = do
  start =<< atomicallyIO (chainUpdate pow)
  where
    --
    start ch = do
      c <- atomicallyIO $ currentConsensus pow
      let (bh, _, _) = _bestHead c
      loop ch =<< mine bh
    --
    loop ch tid = do
      (bh, _) <- awaitIO ch
      liftIO $ killThread tid
      loop ch =<< mine bh
    --
    mine bh = fork $ do
      t <- getCurrentTime
      let blk :: Block Coin
          blk = GBlock { blockHeight = succ $ bhHeight bh
                       , blockTime   = t
                       , prevBlock   = Just $ bhBID bh
                       , blockData   = Coin { coinData   = merkled []
                                            , coinNonce  = 0
                                            , coinTarget = retarget bh
                                            }
                       }
      let find b = (fst <$> adjustPuzzle b) >>= \case
            Just b' -> return b'
            Nothing -> do t' <- getCurrentTime
                          find b { blockTime = t' }
      find blk >>= sendNewBlock pow >>= \case
        Right () -> return ()
        Left  e  -> liftIO $ throwM e


----------------------------------------------------------------
-- Blockchain state management
----------------------------------------------------------------

stateView :: StateView s m Coin
stateView = StateView
  { stateBID          = undefined
  , applyBlock        = undefined
  , revertBlock       = undefined
  , stateComputeAlter = undefined
  , flushState        = undefined
  }
