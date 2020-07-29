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
{-# LANGUAGE ViewPatterns               #-}
-- |
module HSChain.Examples.Coin where

import Control.Concurrent (killThread, threadDelay)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.DeepSeq
import Codec.Serialise      (Serialise)
import qualified Data.Aeson as JSON
import Data.Maybe
import Data.Word
import qualified Database.SQLite.Simple.ToField   as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.FromRow   as SQL
import Katip (LogEnv, Namespace)
import GHC.Generics    (Generic)

import HSChain.Types.Merkle.Types
import HSChain.Control.Class
import HSChain.Control.Channels
import HSChain.Control.Util
import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.Logger
import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.PoW.P2P
import HSChain.Store.Query


----------------------------------------------------------------
-- Monad for running Coin
----------------------------------------------------------------

data CoinDict = CoinDict
  { dictLogEnv    :: !LogEnv
  , dictNamespace :: !Namespace
  , dictConn      :: !(Connection 'RW)
  }
  deriving (Generic)

newtype CoinT m a = CoinT (ReaderT CoinDict m a)
  deriving newtype ( Functor,Applicative,Monad,MonadIO
                   , MonadCatch,MonadThrow,MonadMask,MonadFork)
  deriving (MonadLogger)          via LoggerByTypes  (ReaderT CoinDict m)
  deriving (MonadDB, MonadReadDB) via DatabaseByType (ReaderT CoinDict m)

runCoinT :: LogEnv -> Connection 'RW -> CoinT m a -> m a
runCoinT logenv conn (CoinT act) = runReaderT act (CoinDict logenv mempty conn)

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
    deriving stock    (Show,Generic)
    deriving anyclass (Exception,JSON.ToJSON)

  type Tx Coin = TxCoin

  blockID = CoinID   . hash
  txID    = CoinTxID . hash
  blockTransactions = merkleValue . coinData . blockData

  validateTxContextFree = undefined

  validateHeader bh (Time now) header
    | blockHeight header == 0 = return $ Right ()
    | otherwise               = return $ do
        let Time t = blockTime header
        unless (t <= now + (2*60*60*1000))
          $ Left $ CoinError "Block in far future"
        unless (coinTarget (blockData header) == retarget bh)
          $ Left $ CoinError "Invalid target"
        unless ( blockTargetThreshold header >= hash256AsTarget header)
          $ Left $ CoinError "Not enough work"
  -- At he moment we accept only empty blocks (for simplicity)
  validateBlock b = return $
    case merkleValue $ coinData $ blockData b of
      [] -> Right ()
      _  -> Left $ CoinError "Nonempty block"
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

miningLoop :: MonadFork m => PoW m Coin -> Bool -> m x
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

-- | Database backed state view for the mock coin. This is
--   implementation for archive node.
coinStateView
  :: (MonadThrow m, MonadDB m, MonadIO m)
  => Block Coin
  -> m (BlockDB m Coin, StateView m Coin)
coinStateView genesis = do
  initCoinDB
  storeCoinBlock genesis
  return
    ( BlockDB { storeBlock         = storeCoinBlock
              , retrieveBlock      = retrieveCoinBlock
              , retrieveHeader     = retrieveCoinHeader
              , retrieveAllHeaders = retrieveAllCoinHeaders
              }
    , StateView { stateBID          = undefined
                , applyBlock        = undefined
                , revertBlock       = undefined
                , flushState        = undefined
                }
    )

-- Initialize database for mock coin blockchain
initCoinDB :: (MonadThrow m, MonadDB m, MonadIO m) => m ()
initCoinDB = mustQueryRW $ do
  -- Table for blocks. We store both block data is serialized form and
  -- separately UTXOs for working with blockchain state so data is
  -- duplicated
  basicExecute_
    "CREATE TABLE IF NOT EXISTS coin_blocks \
    \  ( blk_id     INTEGER PRIMARY KEY AUTOINCREMENT \
    \  , bid        BLOB NOT NULL UNIQUE \
    \  , height     INTEGER NOT NULL \
    \  , time       INTEGER NUT NULL \
    \  , prev       BLOB NULL \
    \  , dataHash   BLOB NOT NULL \
    \  , blockData  BLOB NOT NULL \
    \  , target     BLOB NOT NULL \
    \  , nonce      INTEGER NOT NULL \
    \)"
  -- All UTXOs known to blockchain. We never delete them so node works
  -- as archival node.
  basicExecute_
    "CREATE TABLE IF NOT EXISTS coin_utxo \
    \  ( utxo_id INTEGER PRIMARY KEY \
    \  , tx_hash BLOB NOT NULL \
    \  , n_out   BLOB NOT NULL \
    \  , pk_dest BLOB NOT NULL \
    \  , n_coins INTEGER NOT NULL \
    \)"
  -- Changes to UTXO set by given block
  basicExecute_
    "CREATE TABLE IF NOT EXISTS coin_delta \
    \  ( delta_blk INTEGER NOT NULL \
    \  , added     INTEGER NOT NULL \
    \  , removed   INTEGER NOT NULL \
    \  , FOREIGN KEY (delta_blk) REFERENCES coin_blocks(blk_id) \
    \  , FOREIGN KEY (added)     REFERENCES coin_utxo(utxo_id) \
    \  , FOREIGN KEY (removed)   REFERENCES coin_utxo(utxo_id) \
    \)"
  -- Current state of blockchain
  basicExecute_
    "CREATE TABLE IF NOT EXISTS coin_state \
    \  ( live_utxo INTEGER NOT NULL \
    \  , FOREIGN KEY (live_utxo) REFERENCES coin_utxo(utxo_id)\
    \)"
  basicExecute_
    "CREATE TABLE IF NOT EXISTS coin_state_bid \
    \  ( state_block INTEGER NOT NULL \
    \  , uniq        INTEGER NOT NULL UNIQUE \
    \  , FOREIGN KEY (state_block) REFERENCES coin_blocks(blk_id) \
    \  , CHECK (uniq = 0) \
    \)"


retrieveCoinBlock :: (MonadIO m, MonadReadDB m) => BlockID Coin -> m (Maybe (Block Coin))
retrieveCoinBlock bid = queryRO $ basicQueryWith1
  coinBlockDecoder
  "SELECT height, time, prev, blockData, target, nonce FROM coin_blocks WHERE bid = ?"
  (Only bid)

retrieveCoinHeader :: (MonadIO m, MonadReadDB m) => BlockID Coin -> m (Maybe (Header Coin))
retrieveCoinHeader bid = queryRO $ basicQueryWith1
  coinHeaderDecoder
  "SELECT height, time, prev, dataHash, target, nonce FROM coin_blocks WHERE bid = ?"
  (Only bid)

retrieveAllCoinHeaders :: (MonadIO m, MonadReadDB m) => m [Header Coin]
retrieveAllCoinHeaders = queryRO $ basicQueryWith_
  coinHeaderDecoder
  "SELECT height, time, prev, dataHash, target, nonce FROM coin_blocks ORDER BY height"

storeCoinBlock :: (MonadThrow m, MonadIO m, MonadDB m) => Block Coin -> m ()
storeCoinBlock b@GBlock{blockData=Coin{..}, ..} = mustQueryRW $ do
  -- Store block itself
  basicExecute
    "INSERT OR IGNORE INTO coin_blocks VALUES (NULL, ?, ?, ?, ?, ?, ?, ?, ?)"
    ( blockID b
    , blockHeight
    , blockTime
    , prevBlock
    , ByteRepred $ merkleHash coinData
    , CBORed coinData
    , CBORed coinTarget
    , coinNonce
    )
  -- FIXME: expand transactions

coinBlockDecoder :: SQL.RowParser (Block Coin)
coinBlockDecoder = do
  blockHeight <- field
  blockTime   <- field
  prevBlock   <- field
  coinData    <- fieldCBOR
  coinTarget  <- fieldCBOR
  coinNonce   <- field
  return GBlock{ blockData = Coin{..}, ..}

coinHeaderDecoder :: SQL.RowParser (Header Coin)
coinHeaderDecoder = do
  blockHeight <- field
  blockTime   <- field
  prevBlock   <- field
  coinData    <- fromHashed <$> fieldByteRepr
  coinTarget  <- fieldCBOR
  coinNonce   <- field
  return GBlock{ blockData = Coin{..}, ..}
