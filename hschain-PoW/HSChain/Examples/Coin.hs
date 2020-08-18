{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
module HSChain.Examples.Coin where

import Control.Applicative
import Control.Concurrent (killThread, threadDelay)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Except (except)
import Control.Monad.Morph (hoist)
import Control.DeepSeq
import Codec.Serialise      (Serialise)
import Data.Maybe
import Data.Word
import Data.Foldable        (foldl')
import Data.Int
import Data.Map.Strict      (Map)
import Data.List            (nub)
import Data.Generics.Product.Typed (typed)
import qualified Data.Aeson                       as JSON
import qualified Data.Map.Strict                  as Map
import           Database.SQLite.Simple            ((:.)(..))
import qualified Database.SQLite.Simple.ToField   as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.FromRow   as SQL
import qualified Database.SQLite.Simple.ToRow     as SQL
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
import HSChain.PoW.BlockIndex
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

  data BlockException Coin
    = CoinError    String
    | CoinInternal String
    deriving stock    (Show,Generic)
    deriving anyclass (Exception,JSON.ToJSON)

  type Tx Coin = TxCoin

  blockID = CoinID   . hash
  txID    = CoinTxID . hash
  blockTransactions = merkleValue . coinData . blockData

  validateTxContextFree = validateCoinTxContextFree

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


instance SQL.FromRow Unspent where
  fromRow = Unspent <$> fieldByteRepr <*> SQL.field
instance SQL.ToRow Unspent where
  toRow (Unspent k n) = [SQL.toField (ByteRepred k), SQL.toField n]

instance SQL.FromRow UTXO where
  fromRow = UTXO <$> field <*> fieldByteRepr
instance SQL.ToRow UTXO where
  toRow (UTXO n h) = [SQL.toField n, SQL.toField (ByteRepred h)]


----------------------------------------
-- Tx validation


-- | Context free TX validation for normal (not coinbase)
--   transactions. This function performs all checks that could be
--   done having only transaction at hand.
validateCoinTxContextFree :: TxCoin -> Either (BlockException Coin) ()
validateCoinTxContextFree (TxCoin pubK sig txSend@TxSend{..}) = do
  -- Inputs and outputs are not null
  when (null txInputs)  $ Left $ CoinError "Empty input list"
  when (null txOutputs) $ Left $ CoinError "Empty output list"
  -- No duplicate inputs
  when (nub txInputs /= txInputs) $ Left $ CoinError "Duplicate inputs"
  -- Outputs are all positive
  forM_ txOutputs $ \(Unspent _ n) ->
    unless (n > 0) $ Left $ CoinError "Negative output"
  -- Signature must be valid.
  unless (verifySignatureHashed pubK txSend sig)
    $ Left $ CoinError "Invalid signature"


-- | Finally process transaction. Check that sum of inputs is same as sum of outputs and
processTX
  :: BlockIndexPath (ID (Block Coin))
  -> ActiveOverlay
  -> TxCoin
  -> ExceptT (BlockException Coin) (Query rw) ActiveOverlay
processTX pathInDB overlay tx@(TxCoin pk _ (TxSend ins outs)) = do
  -- Fetch all inputs
  inputs <- forM ins $ \utxo -> do
    case getOverlayUTXO overlay utxo of
      Just (Spent _) -> throwError $ CoinError "Input already spent"
      Just (Added u) -> return (utxo,u)
      Nothing        -> (,) utxo <$> getDatabaseUTXO pathInDB utxo
  -- Check that we can spend them
  let canSpend  = and [ pk == k | (_, Unspent k _) <- inputs ]
      sumInputs = sum [ n       | (_, Unspent _ n) <- inputs ]
      sumOuts   = sum [ n       | Unspent _ n      <- outs   ]
  unless canSpend               $ throwError $ CoinError "Can't spend output"
  unless (sumInputs == sumOuts) $ throwError $ CoinError "In/out sum mismatch"
  -- Update overlay
  let txHash   = hashed tx
      overlay1 = foldl' (\o (utxo,u) -> spendUTXO  utxo u o) overlay inputs
      overlay2 = foldl' (\o (utxo,u) -> createUTXO utxo u o) overlay1
        [ (UTXO i txHash, o)
        | (i,o) <- [0..] `zip` outs
        ]
  return overlay2

getDatabaseUTXO
  :: ()
  => BlockIndexPath (ID (Block Coin))
  -> UTXO
  -> ExceptT (BlockException Coin) (Query rw) Unspent
-- Check whether output was created in the block
getDatabaseUTXO (ApplyBlock i path) utxo = do
  isSpentAtBlock i utxo >>= \case
    Just _  -> throwError $ CoinError "Output is already spent"
    Nothing -> return ()
  isCreatedAtBlock i utxo >>= \case
    Just u  -> return u
    Nothing -> getDatabaseUTXO path utxo
-- Perform check in block being reverted. If UTXO was create in that
-- block it didn't exist before and we should abort.
getDatabaseUTXO (RevertBlock i path) utxo = do
  isCreatedAtBlock i utxo >>= \case
    Just _  -> throwError $ CoinError "Output does not exists"
    Nothing -> return ()
  isSpentAtBlock i utxo >>= \case
    Just u  -> return u
    Nothing -> getDatabaseUTXO path utxo
-- Read from database
getDatabaseUTXO NoChange utxo = do
  r <- basicQuery1
    "SELECT pk_dest, n_coins \
    \  FROM coin_utxo \
    \  JOIN coin_state ON live_utxo = utxo_id \
    \ WHERE tx_hash = ? AND n_out = ?"
    utxo
  case r of
    Just u  -> return u
    Nothing -> throwError $ CoinError "No such UTXO"

isSpentAtBlock :: MonadQueryRO m => ID (Block Coin) -> UTXO -> m (Maybe Unspent)
isSpentAtBlock i utxo = basicQuery1
  "SELECT pk_dest, n_coins \
  \  FROM coin_utxo \
  \  JOIN coin_utxo_spent ON utxo_id = utxo_ref \
  \ WHERE tx_hash = ? AND n_out = ? AND block_ref = ?"
  (utxo :. Only i)

isCreatedAtBlock :: MonadQueryRO m => ID (Block Coin) -> UTXO -> m (Maybe Unspent)
isCreatedAtBlock i utxo = basicQuery1
  "SELECT pk_dest, n_coins \
  \  FROM coin_utxo \
  \  JOIN coin_utxo_created ON utxo_id = utxo_ref \
  \ WHERE tx_hash = ? AND n_out = ? AND block_ref = ?"
  (utxo :. Only i)


----------------------------------------------------------------
-- Blockchain state management
----------------------------------------------------------------

-- | In-memory overlay for coin state. It contain changes to UTXO set
--   that are not commited to the database.
--
--   Note that it only contains block that are added to blockchain but
--   not rollbacks since latter are already commited.
data StateOverlay
  = OverlayBase  (BH Coin)
  | OverlayLayer (BH Coin) Layer StateOverlay

-- | Overlay that is guaranteed to have layer to add UTXOs
data ActiveOverlay = ActiveOverlay (BH Coin) Layer StateOverlay
  deriving Generic

-- | Changes to database set generated by single block
data Layer = Layer
  { utxoCreated :: Map UTXO Unspent
  , utxoSpent   :: Map UTXO Unspent
  }

lensCreated, lensSpent :: Lens' Layer (Map UTXO Unspent)
lensCreated = lens utxoCreated (\m x -> m { utxoCreated = x })
lensSpent   = lens utxoSpent   (\m x -> m { utxoSpent   = x })

-- | Change to UTXO set
data Change a
  = Added a
  | Spent a

-- | Create empty overlay build over given block. It correspond to
--   state after evaluation of that block.
emptyOverlay :: BH Coin -> StateOverlay
emptyOverlay = OverlayBase

-- | Get pointer to block index to block over which overlay is built.
overlayBase :: StateOverlay -> BH Coin
overlayBase (OverlayBase  bh)    = bh
overlayBase (OverlayLayer _ _ o) = overlayBase o

overlayTip :: StateOverlay -> BH Coin
overlayTip (OverlayBase  bh)     = bh
overlayTip (OverlayLayer bh _ _) = bh

-- | Roll back overlay by one block. Will throw if rolling past
--   genesis is attempted.
rollbackOverlay :: StateOverlay -> StateOverlay
rollbackOverlay (OverlayBase bh0) = case bhPrevious bh0 of
  Just bh -> OverlayBase bh
  Nothing -> error "Cant rewind overlay pas genesis"
rollbackOverlay (OverlayLayer _ _ o) = o

-- | Add overlayt layer. Will return nothing if new block doesn't
--   point to current state of overlay
addOverlayLayer :: BH Coin -> StateOverlay -> Maybe ActiveOverlay
addOverlayLayer bh o = do
  bid <- bhBID <$> bhPrevious bh
  guard $ bhBID (overlayTip o) == bid
  Just  $ ActiveOverlay bh (Layer mempty mempty) o

finalizeOverlay :: ActiveOverlay -> StateOverlay
finalizeOverlay (ActiveOverlay bh l o) = OverlayLayer bh l o

-- | Find whether given UTXO is awaialble to be spent or spent
--   already. We need latter since UTXO could be available in
--   underlying state but spent in overlay and we need to account for
--   that explicitly.
getOverlayUTXO :: ActiveOverlay -> UTXO -> Maybe (Change Unspent)
getOverlayUTXO (ActiveOverlay _ l0 o0) utxo
  =  getFromLayer l0
 <|> recur o0
 where
   recur (OverlayBase  _)     = Nothing
   recur (OverlayLayer _ l o) =  getFromLayer l
                             <|> recur o
   getFromLayer Layer{..}
     =  Spent <$> Map.lookup utxo utxoSpent
    <|> Added <$> Map.lookup utxo utxoCreated


spendUTXO :: UTXO -> Unspent -> ActiveOverlay -> ActiveOverlay
spendUTXO utxo val
  = typed . lensSpent . at utxo .~ Just val

createUTXO :: UTXO -> Unspent -> ActiveOverlay -> ActiveOverlay
createUTXO utxo val
  = typed . lensCreated . at utxo .~ Just val

dumpOverlay :: MonadQueryRW m => StateOverlay -> m ()
dumpOverlay (OverlayLayer bh Layer{..} o) = do
  -- Store create UTXO
  forM_ (Map.toList utxoCreated) $ \(utxo, unspent) -> do
    basicExecute
      "INSERT OR IGNORE INTO coin_utxo VALUES (NULL,?,?,?,?)"
      (utxo :. unspent)
  -- Write down block delta
  bid <- retrieveCoinBlockTableID (bhBID bh)
  forM_ (Map.keys utxoCreated) $ \utxo -> do
    uid <- retrieveUtxoIO utxo
    basicExecute
      "INSERT OR IGNORE INTO coin_utxo_added VALUES (?,?)"
      (bid, uid)
  forM_ (Map.keys utxoSpent) $ \utxo -> do
    uid <- retrieveUtxoIO utxo
    basicExecute
      "INSERT OR IGNORE INTO coin_utxo_spent VALUES (?,?)"
      (bid, uid)
  dumpOverlay o
dumpOverlay OverlayBase{} = return ()

revertBlockDB :: MonadQueryRW m => BH Coin -> m ()
revertBlockDB bh = do
  i <- retrieveCoinBlockTableID $ bhBID bh
  basicExecute
    "DELETE FROM coin_state WHERE live_utxo IN \
    \  SELECT utxo_ref FROM coin_utxo_created WHERE block_ref = ?"
    (Only i)
  basicExecute
    "INSERT INTO coin_state \
    \  SELECT utxo_ref FROM coin_utxo_spent WHERE block_ref = ?"
    (Only i)

applyBlockDB :: MonadQueryRW m => BH Coin -> m ()
applyBlockDB bh = do
  i <- retrieveCoinBlockTableID $ bhBID bh
  basicExecute
    "DELETE FROM coin_state WHERE live_utxo IN \
    \  SELECT utxo_ref FROM coin_utxo_spent WHERE block_ref = ?"
    (Only i)
  basicExecute
    "INSERT INTO coin_state \
    \  SELECT utxo_ref FROM coin_utxo_created WHERE block_ref = ?"
    (Only i)

-- | Database backed state view for the mock coin. This is
--   implementation for archive node.
coinStateView
  :: (MonadThrow m, MonadDB m, MonadIO m, MonadDB m)
  => Block Coin
  -> m (BlockDB m Coin, BlockIndex Coin, StateView m Coin)
coinStateView genesis = do
  initCoinDB
  storeCoinBlock genesis
  bIdx <- buildBlockIndex db
  st   <- mustQueryRW $ initializeStateView genesis bIdx
  return (db, bIdx, st)
  where
    db = BlockDB { storeBlock         = storeCoinBlock
                 , retrieveBlock      = retrieveCoinBlock
                 , retrieveHeader     = retrieveCoinHeader
                 , retrieveAllHeaders = retrieveAllCoinHeaders
                 }

initializeStateView
  :: (MonadDB m, MonadThrow m, MonadQueryRW q,  MonadIO m)
  => Block Coin -> BlockIndex Coin -> q (StateView m Coin)
initializeStateView genesis bIdx = do
  retrieveCurrentStateBlock >>= \case
    Just bid -> do let Just bh = lookupIdx bid bIdx
                   return $ makeStateView bIdx (emptyOverlay bh)
    -- We need to initialize state table
    Nothing  -> do
      let bid     = blockID genesis
          Just bh = lookupIdx bid bIdx
      basicExecute
        "INSERT INTO coin_state_bid SELECT blk_id,0 FROM coin_blocks WHERE bid = ?"
        (Only bid)
      return $ makeStateView bIdx (emptyOverlay bh)      

makeStateView
  :: (MonadDB m, MonadThrow m, MonadIO m)
  => BlockIndex Coin -> StateOverlay -> StateView m Coin
makeStateView bIdx0 overlay = sview where
  bh0   = overlayTip overlay
  sview = StateView
    { stateBID          = bhBID bh0
    -- FIXME: We need block index in order to be able to compute path
    --        from state to last known state
    , applyBlock        = \bIdx bh b -> runExceptT $ do
        -- Consistency checks
        unless (bhPrevious bh == Just bh0)  $ throwError $ CoinInternal "BH mismatich"
        unless (bhBID bh      == blockID b) $ throwError $ CoinInternal "BH don't match block"
        --
        let txList = merkleValue $ coinData $ blockData b
        -- Perform context free validation of all transactions in
        -- block
        () <- except
            $ mapM_ (validateTxContextFree @Coin) txList
        -- Now we need to fully verify each transaction and build new
        -- overlay for database
        overlay' <- hoist mustQueryRW $ do
          -- First we need to prepare path between block corresponding
          -- to current state of block
          pathInDB <- do
            Just stateBid <- retrieveCurrentStateBlock
            let Just bhState = lookupIdx stateBid bIdx
            makeBlockIndexPathM (retrieveCoinBlockTableID . bhBID)
              bhState (overlayBase overlay)
          -- Now we can just validate every TX and update overlay
          foldM
            (processTX pathInDB)
            (fromMaybe (error "Coin: invalid BH in apply block") $ addOverlayLayer bh overlay)
            txList
        return $ makeStateView bIdx (finalizeOverlay overlay')
    --
    , revertBlock = return $ makeStateView bIdx0 (rollbackOverlay overlay)
    --
    , flushState = mustQueryRW $ do
        -- Dump overlay content.
        dumpOverlay overlay
        -- Rewind state stored in the database from its current state
        -- to current head.
        Just bid <- retrieveCurrentStateBlock
        case bid `lookupIdx` bIdx0 of
          Nothing -> error "makeStateView: bad index"
          Just bh -> traverseBlockIndexM_ revertBlockDB applyBlockDB bh bh0
        return $ makeStateView bIdx0 (emptyOverlay bh0)
      -- FIXME: not implemented
    , checkTx                  = undefined
    , createCandidateBlockData = undefined
    }

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
  -- UTXO's created in given block. Due to blockchain reorganizations
  -- same UTXO may appear in several blocks
  basicExecute_
    "CREATE TABLE IF NOT EXISTS coin_utxo_created \
    \  ( block_ref INTEGER NOT NULL \
    \  , utxo_ref  INTEGER NOT NULL \
    \  , FOREIGN KEY (block_ref) REFERENCES coin_blocks(blk_id) \
    \  , FOREIGN KEY (utxo_ref)  REFERENCES coin_utxo(utxo_id)  \
    \  , UNIQUE (block_ref, utxo_ref) \
    \)"
  basicExecute_
    "CREATE TABLE IF NOT EXISTS coin_utxo_spent \
    \  ( block_ref INTEGER NOT NULL \
    \  , utxo_ref  INTEGER NOT NULL \
    \  , FOREIGN KEY (block_ref) REFERENCES coin_blocks(blk_id) \
    \  , FOREIGN KEY (utxo_ref)  REFERENCES coin_utxo(utxo_id)  \
    \  , UNIQUE (block_ref, utxo_ref) \
    \)"
  -- Current state of blockchain. It's just set of currently live UTXO
  -- with pointer to the block for which it corresponds
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

retrieveCurrentStateBlock :: MonadQueryRO m => m (Maybe (BlockID Coin))
retrieveCurrentStateBlock = fmap fromOnly <$> basicQuery1
  "SELECT bid \
  \  FROM coin_blocks \
  \  JOIN coin_state_bid ON state_block = blk_id"
  ()

retrieveCoinBlockTableID :: MonadQueryRO m => BlockID Coin -> m (ID (Block Coin))
retrieveCoinBlockTableID bid = do
  r <- basicQuery1
    "SELECT blk_id FROM coin_blocks WHERE bid =?"
    (Only bid)
  case r of
    Nothing       -> error "Unknown BID"
    Just (Only i) -> return i

retrieveUtxoIO :: MonadQueryRO m => UTXO -> m Int64
retrieveUtxoIO utxo = do
  r <- basicQuery1
    "SELECT utxo_id FROM coin_utxo WHERE tx_hash = ? AND n_out = ?"
    utxo
  case r of
    Just (Only i) -> return i
    Nothing       -> error "retrieveUtxoIO"


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
