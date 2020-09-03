{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Very simple UTXO coin intended for demonstration of hschain. As a
-- demonstration it doesn't have any sort of economics nor in form of
-- block reward, nor in form of transaction fee. Spend scripts are
-- primitive as well. All that's possible is to send money to owner of
-- private key
module HSChain.Mock.Coin (
    Alg
  , TxSend(..)
  , Tx(..)
  , BData(..)
    -- * Pure state
  , CoinState(..)
  , Unspent(..)
  , UTXO(..)
  , inMemoryStateView
    -- ** Transaction generator
  , TxGenerator(..)
  , makeCoinGenerator
  , transactionGenerator
  , generateTransaction
    -- * In-DB state
  , initCoinDB
  , databaseStateView
    -- * Interpretation
    -- ** Monad
  , CoinT(..)
  , runCoinT
  , CoinDictM(..)
  , CoinSpecification(..)
    -- ** Executing specification
  , coinGenesis
  , interpretSpec
  , executeNodeSpec
  ) where

import Codec.Serialise
import Control.Arrow (second)
import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Except (except)
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Trans.Cont
import Control.Monad.Morph (hoist)
import qualified Data.Aeson as JSON
import Data.Foldable
import Data.Either
import Data.IORef
import Data.Maybe
import Data.Map             (Map,(!))
import Database.SQLite.Simple             ((:.)(..))
import qualified Database.SQLite.Simple.ToField   as SQL
import qualified Database.SQLite.Simple.FromRow   as SQL
import qualified Database.SQLite.Simple.ToRow     as SQL
import qualified Data.Vector         as V
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import Katip           (Namespace,LogEnv)
import qualified System.Random.MWC as MWC
import GHC.Generics    (Generic)

import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
import HSChain.Types.Validators
import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control.Class
import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Logger
import HSChain.Mempool
import HSChain.Run
import HSChain.Internal.Types.Consensus
import HSChain.Mock
import HSChain.Mock.Coin.Types
import HSChain.Store
import HSChain.Store.Internal.Query
import HSChain.Store.Query  hiding (queryRO, queryRW, mustQueryRW)
import HSChain.Mock.KeyList         (makePrivKeyStream)
import HSChain.Mock.Types
import HSChain.Monitoring
import qualified HSChain.Network.Mock as P2P
import qualified HSChain.Config


----------------------------------------------------------------
-- State management
----------------------------------------------------------------

-- | Context free TX validation. This function performs all checks
--   that could be done having only transaction at hand.
validateTxContextFree :: Tx -> Either CoinError ()
validateTxContextFree (Deposit _ n)
  | n > 0 = return ()
  | otherwise = Left $ CoinError "Negative deposit"
validateTxContextFree (Send pubK sig txSend@TxSend{..}) = do
  -- Inputs and outputs are not null
  when (null txInputs)  $ Left $ CoinError "Empty input  list"
  when (null txOutputs) $ Left $ CoinError "Empty output list"
  -- Outputs are all positive
  forM_ txOutputs $ \(Unspent _ n) ->
    unless (n > 0) $ Left $ CoinError "Negative output"
  -- Signature must be valid.
  unless (verifySignatureHashed pubK txSend sig)
    $ Left $ CoinError "Invalid signature"



----------------------------------------------------------------
-- In memory state handling
----------------------------------------------------------------

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

instance CryptoHashable CoinState where
  hashStep = genericHashStep "hschain"


-- | Create view on blockchain state which is kept completely in
--   memory
inMemoryStateView
  :: MonadIO m
  => CoinSpecification
  -> ValidatorSet (Alg BData)
  -> m (StateView m BData, [m ()], IO CoinState)
inMemoryStateView CoinSpecification{coinMaxBlockSize} valSet0 = do
  varSt <- liftIO $ newIORef $ CoinState mempty mempty
  (mem@Mempool{..}, memThr) <- newMempool hashed (isRight . validateTxContextFree)
  let make mh vals txList st = r where
        r = StateView
          { stateHeight       = mh
          , newValidators     = vals
          -- For commit we simply remove transaction in block from
          -- mempool and asking it to start filtering
          , commitState       = do
              removeTxByHashes $ hashed <$> txList
              startMempoolFiltering $ \tx -> return $ isRight $ processSend tx st
              liftIO $ writeIORef varSt st
              return r
          -- When we validate proposed block we want to do complete
          -- validation since block is sent from outside
          , validatePropBlock = \b valSet -> do
              let txs = unBData $ merkleValue $ blockData b
                  step s tx = do
                    validateTxContextFree tx
                    processTxFull (blockHeight b) tx s
              return $ do
                when (blockHeight b > Height 0 && length txs > coinMaxBlockSize) $
                  Left $ CoinError "Block is too big"
                st' <- foldM step st txs
                return $ make (Just $ blockHeight b) valSet txs st'
          --
          , generateCandidate = \NewBlock{..} -> do
              memSt  <- getMempoolState
              let selectTx 0 c _      = (c,[])
                  selectTx _ c []     = (c,[])
                  selectTx n c (t:tx) = case processSend t c of
                    Left  _  -> selectTx n c tx
                    Right c' -> let (c'', b  ) = selectTx (n-1) c' tx
                                in  (c'', t:b)
              let (st', dat) = selectTx coinMaxBlockSize st
                             $ toList memSt
              return
                ( BData dat
                , make (Just newBlockHeight) newBlockValSet dat st'
                )
          , stateMempool      = mem
          }
  return ( make Nothing valSet0 [] (CoinState mempty mempty)
         , [memThr]
         , readIORef varSt
         )

-- |Process transaction performing complete validation.
processTxFull :: Height -> Tx -> CoinState -> Either CoinError CoinState
processTxFull (Height 0) = processDeposit
processTxFull _          = processSend

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
processSend :: Tx -> CoinState -> Either CoinError CoinState
processSend Deposit{} _ = Left DepositAtWrongH
processSend transaction@(Send pubK _ TxSend{..}) CoinState{..} = do
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


----------------------------------------------------------------
-- Transaction generator
----------------------------------------------------------------

-- | Specification of generator of transactions
data TxGenerator = TxGenerator
  { genPrivateKeys    :: V.Vector (PrivKey (Alg BData), PublicKey (Alg BData))
    -- ^ Private keys for which we can generate transactions
  , genDelay          :: Int
    -- ^ Delay between invokations of generator
  , genMaxMempoolSize :: Int
  , genPRNG           :: MWC.GenIO
  }

-- | Create generator for coin transactions
makeCoinGenerator
  :: MonadIO m
  => CoinSpecification
  -> m (Maybe TxGenerator)
makeCoinGenerator CoinSpecification{..} = liftIO $ do
  genPRNG  <- MWC.createSystemRandom
  return $ do genDelay <- coinGeneratorDelay
              return TxGenerator
                { genPrivateKeys    = V.fromList [ (k, publicKey k) | k <- privK ]
                , genMaxMempoolSize = coinMaxMempoolSize
                , ..
                }
  where
    privK = take coinWallets $ makePrivKeyStream coinWalletsSeed


-- | Run generator for transactions
transactionGenerator
  :: MonadIO m
  => TxGenerator
  -> Mempool m (Hashed (Alg BData) Tx) Tx
  -> m CoinState
  -> (Tx -> m ())
  -> m a
transactionGenerator gen mempool coinState push = forever $ do
  size <- mempoolSize mempool
  when (maxN > 0 && size < maxN) $
    push =<< generateTransaction gen =<< coinState
  liftIO $ threadDelay $ genDelay gen * 1000
  where
    maxN = genMaxMempoolSize gen

generateTransaction :: MonadIO m => TxGenerator -> CoinState -> m Tx
generateTransaction TxGenerator{..} CoinState{..} = liftIO $ do
  (privK,pubK) <- selectFromVec genPRNG genPrivateKeys
  (_,target)   <- selectFromVec genPRNG genPrivateKeys
  amount <- fromIntegral <$> MWC.uniformR (1,20::Int) genPRNG
  let allInputs = toList
                $ fromMaybe Set.empty
                $ pubK `Map.lookup` utxoLookup
      inputs    = findInputs amount [ (utxo, n)
                                    | utxo <- allInputs
                                    , let Unspent _ n = unspentOutputs ! utxo
                                    ]
      avail     = sum (snd <$> inputs)
      change    = avail - amount
      outs | change < 0 = [ Unspent target avail]
           | otherwise  = [ Unspent target amount
                          , Unspent pubK   change
                          ]
      tx = TxSend { txInputs  = map fst inputs
                  , txOutputs = outs
                  }
  return $ Send pubK (signHashed privK tx) tx

selectFromVec :: MWC.GenIO -> V.Vector a -> IO a
selectFromVec gen v = do
  i <- MWC.uniformR (0, V.length v - 1) gen
  return $ v V.! i

findInputs :: (Num i, Ord i) => i -> [(a,i)] -> [(a,i)]
findInputs tgt = go 0
  where go _ [] = []
        go acc ((tx,i):rest)
          | acc' >= tgt = [(tx,i)]
          | otherwise   = (tx,i) : go acc' rest
          where
            acc' = acc + i


----------------------------------------------------------------
-- State stored in database
----------------------------------------------------------------

instance SQL.FromRow Unspent where
  fromRow = Unspent <$> fieldByteRepr <*> SQL.field
instance SQL.ToRow Unspent where
  toRow (Unspent k n) = [ SQL.toField (ByteRepred k), SQL.toField n ]

instance SQL.FromRow UTXO where
  fromRow = UTXO <$> field <*> fieldByteRepr
instance SQL.ToRow UTXO where
  toRow (UTXO n h) = [SQL.toField n, SQL.toField (ByteRepred h)]

-- | Initialize tables for storage of coin state. Storage is heavily
--   tailored towards particular use case of demonstartion and
--   benchmarks for mock UTXO blockchain and likely won't work well
--   for more realistic case.
initCoinDB :: MonadQueryRW m => m ()
initCoinDB = do
  -- Table for unspent transactions. In order to be able to work with
  -- several instances of StateView we keep heights when UTXO was
  -- creted and spent
  basicExecute_
    "CREATE TABLE IF NOT EXISTS coin_utxo \
    \  ( n_out   INTEGER NOT NULL \
    \  , tx_hash BLOB    NOT NULL \
    \  , pk      BLOB    NOT NULL \
    \  , n_coins INTEGER NOT NULL \
    \  , h_added INTEGER NOT NULL \
    \  , h_spent INTEGER NULL     \
    \  , UNIQUE (tx_hash,n_out)   \
    \  , FOREIGN KEY (tx_hash) REFERENCES coin_pk(id))"
  basicExecute_
    "CREATE INDEX coin_utxo_idx_txgen ON coin_utxo(pk, h_spent IS NULL)"

data UtxoChange
  = Spent !Height
    -- ^ Already existing UTXO is spent at given H
  | Added !Unspent !Height
    -- ^ UTXO was created at given H
  | Both  !Unspent !Height !Height
    -- ^ UTXO was created and then spent

-- | State difference
data UtxoDiff = UtxoDiff
  { baseH    :: Height              -- ^ Height which is already commited in the database
  , utxoDiff :: Map UTXO UtxoChange -- ^ Differences relative to baseH
  }

dbProcessDeposit
  :: (Monad m)
  => Tx -> UtxoDiff -> ExceptT CoinError m UtxoDiff
dbProcessDeposit Send{} _ = throwError $ UnexpectedSend
dbProcessDeposit tx@(Deposit pk nCoin) UtxoDiff{..} =
  return $ UtxoDiff baseH $ Map.insert utxo (Added (Unspent pk nCoin) (Height 0)) utxoDiff
  where
    utxo = UTXO 0 (hashed tx)

dbProcessSend
  :: (MonadQueryRO m)
  => CoinStatements -> Height -> Tx -> UtxoDiff -> ExceptT CoinError m UtxoDiff
dbProcessSend _ _ Deposit{} _ = throwError DepositAtWrongH
dbProcessSend CoinStatements{..} h tx@(Send pk _ TxSend{..}) UtxoDiff{..} = do
  -- Try to find all inputs for transaction
  inputs <- forM txInputs $ \utxo -> do
    Unspent pk' n <- case utxo `Map.lookup` utxoDiff of
      Just (Added u _) -> return u
      Just _           -> throwError $ CoinError "Already spent output"
      Nothing -> preparedQuery1 stmtLookupUTXO (utxo :. Only baseH) >>= \case
        Nothing -> throwError $ CoinError "Already spent output"
        Just x  -> return x
    unless (pk == pk') $ throwError $ CoinError "PublicKey mismatch"
    return n
  -- Check that sum of inputs and outputs match
  unless (sum inputs == sum [n | Unspent _ n <- txOutputs])
    $ throwError $ CoinError "Missmatch between inputs and outputs"
  -- Update diff
  return
    $ UtxoDiff baseH
    $ (\m -> foldl' addOutput  m ([0..] `zip` txOutputs))
    $ (\m -> foldl' spendInput m txInputs)
    $ utxoDiff
  where
    txHash = hashed tx
    spendInput m utxo = Map.alter
      (\case
          Nothing           -> Just $ Spent h
          Just (Added u h0) -> Just $ Both u h0 h
          Just _            -> error "Coin: internal error"
      ) utxo m
    addOutput m (i,u) = Map.insert (UTXO i txHash) (Added u h) m

dbRecordDiff
  :: (MonadQueryRW m)
  => CoinStatements -> (UTXO, UtxoChange) -> m ()
dbRecordDiff CoinStatements{..} (utxo, change) = case change of
  Spent h         -> preparedExecute stmtSpendExisting (Only h :. utxo)
  Added out h     -> preparedExecute stmtInsertUnspent  (utxo :. out :. Only h)
  Both  out h1 h2 -> preparedExecute stmtInsertSpent    (utxo :. out :. (h1,h2))


data CoinStatements = CoinStatements
  { stmtLookupUTXO    :: PreparedQuery (UTXO :. Only Height) Unspent
    -- ^ Query for lookup of UTXO
  , stmtInsertSpent   :: PreparedStmt (UTXO :. Unspent :. (Height, Height))
    -- ^ Insert already spent UTXO
  , stmtInsertUnspent :: PreparedStmt (UTXO :. Unspent :. Only Height)
    -- ^ Insert unspent UTXO
  , stmtSpendExisting :: PreparedStmt (Only Height :. UTXO)
    -- ^ Spend existing UTXO
  , stmtGenerateUTXO  :: PreparedQuery (Only (ByteRepred (PublicKey (Alg BData))))
                                       (UTXO :. Only Integer)
    -- ^ Query for genrating transactions
  }

newCoinStatements :: (MonadMask m, MonadIO m, MonadDB m) => m CoinStatements
newCoinStatements = do
  stmtLookupUTXO <- prepareQuery
    "SELECT pk,n_coins FROM coin_utxo \
    \ WHERE n_out = ? AND tx_hash = ?\
    \   AND (h_spent is NULL OR h_spent >= ?)"
  --
  stmtInsertSpent   <- prepareStatement "INSERT INTO coin_utxo VALUES (?,?,?,?,?,?)" 
  stmtInsertUnspent <- prepareStatement "INSERT INTO coin_utxo VALUES (?,?,?,?,?,NULL)"
  stmtSpendExisting <- prepareStatement "UPDATE coin_utxo SET h_spent=? WHERE n_out=? AND tx_hash=?"
  --
  stmtGenerateUTXO <- prepareQuery
    "SELECT n_out, tx_hash, n_coins \
    \  FROM coin_utxo \
    \ WHERE pk=? AND h_spent IS NULL \
    \ ORDER BY random() LIMIT 20"
  return CoinStatements{..}

databaseStateView
  :: (MonadIO m, MonadMask m, MonadDB m, MonadCached BData m)
  => CoinSpecification
  -> ValidatorSet (Alg BData)
  -> m (StateView m BData, [m ()], CoinStatements)
databaseStateView CoinSpecification{coinMaxBlockSize} valSetH0 = do
  stmt <- newCoinStatements
  (mem@Mempool{..}, memThr) <- newMempool hashed (isRight . validateTxContextFree)
  -- First we find what is latest height at which we updated state and
  -- use it as startign point for our state management.
  (h0,valSet0) <- queryRO $ do
    [h] <- fmap fromOnly <$> basicQuery "SELECT MAX(h_added) FROM coin_utxo" ()
    v   <- retrieveValidatorSet $ succH h
    return (h, fromMaybe valSetH0 v)
  -- Create state view. Parameters meaning:
  --
  --   - viewH  - which height is already written to database
  --   - txList - list of transcation to remove duting commit
  --   - vals   - validator set after
  let make stateH txList vals diff = sview where
        sview = StateView
          { stateHeight   = stateH
          , newValidators = vals
          --
          , commitState   = case stateH of
              Nothing -> return sview
              Just h  -> do
                -- Filter mempool
                removeTxByHashes $ hashed <$> txList
                mustQueryRW $ do
                  mapM_ (dbRecordDiff stmt) $ Map.toList $ utxoDiff diff
                -- We ask mempool to start filtering TX after we done writing
                startMempoolFiltering $ \tx ->
                  fmap isRight $ queryRO $ runExceptT $ dbProcessSend stmt (succ h) tx diff
                return $ make stateH [] vals (UtxoDiff (succH stateH) Map.empty)
          --
          , validatePropBlock = \b valSet -> do
              let step d tx
                    | h == Height 0 = dbProcessDeposit tx d
                    | otherwise     = do
                        except $ validateTxContextFree tx
                        dbProcessSend stmt h tx d
                    where
                      h = blockHeight b
              let txs = unBData $ merkleValue $ blockData b
              runExceptT $ do
                when (blockHeight b > Height 0 && length txs > coinMaxBlockSize) $ throwError $
                  CoinError $ "Block too big: " ++ show (length txs) ++ "/" ++ show coinMaxBlockSize
                diff' <- hoist queryRO $ foldM step diff txs
                return $ make (Just $ blockHeight b) txs valSet diff'
          --
          , generateCandidate = \NewBlock{..} -> do
              let selectTx 0 d _  = return (d, [])
                  selectTx _ d [] = return (d,[])
                  selectTx n d (t:tx) =
                    runExceptT (dbProcessSend stmt newBlockHeight t d) >>= \case
                      Left  _  -> selectTx n d tx
                      Right d' -> second (t:) <$> selectTx (n-1) d' tx
              --
              memSt <- getMempoolState
              (diff',txs) <- queryRO $ selectTx coinMaxBlockSize diff $ toList memSt
              return ( BData txs
                     , make (Just newBlockHeight) txs newBlockValSet diff'
                     )
          -- Mempool
          , stateMempool = mem
          }
  -- Read
  return ( make h0 [] valSet0 (UtxoDiff (succH h0) Map.empty)
         , [memThr]
         , stmt
         )
  where
    succH = maybe (Height 0) succ



-- | Run generator for transactions
dbTransactionGenerator
  :: (MonadReadDB m, MonadCached BData m, MonadIO m)
  => CoinStatements
  -> TxGenerator
  -> Mempool m (Hashed (Alg BData) Tx) Tx
  -> (Tx -> m ())
  -> m a
dbTransactionGenerator dict gen mempool push = forever $ do
  size <- mempoolSize mempool
  when (maxN > 0 && size < maxN) $
    push =<< dbGenerateTransaction dict gen
  liftIO $ threadDelay $ genDelay gen * 1000
  where
    maxN = genMaxMempoolSize gen


dbGenerateTransaction
  :: (MonadIO m, MonadReadDB m, MonadCached BData m)
  => CoinStatements -> TxGenerator -> m Tx
dbGenerateTransaction CoinStatements{..} TxGenerator{..} = do
  (privK,pubK) <- liftIO $ selectFromVec genPRNG genPrivateKeys
  (_,target)   <- liftIO $ selectFromVec genPRNG genPrivateKeys
  amount       <- liftIO $ fromIntegral <$> MWC.uniformR (1,20::Int) genPRNG
  inputs <- queryRO $ preparedQueryScanl1 stmtGenerateUTXO (Only (ByteRepred pubK))
    (\(u :. Only n) acc -> case acc + n of
        acc' | acc' >= 20 -> Left  (u,n)
             | otherwise  -> Right ((u,n), acc')
    ) 0
  let avail  = sum (snd <$> inputs)
      change = avail - amount
      outs | change == 0 = [ Unspent target avail]
           | otherwise   = [ Unspent target amount
                           , Unspent pubK   change
                           ]
      tx = TxSend { txInputs  = map fst inputs
                  , txOutputs = outs
                  }
  return $ Send pubK (signHashed privK tx) tx


----------------------------------------------------------------
-- Interpretation of coin
----------------------------------------------------------------

-- | Specifications for mock coin status. It specify both genesis
--   block and optionally transaction generator.
data CoinSpecification = CoinSpecification
 { coinAirdrop        :: !Integer
   -- ^ Amount of coins allocated to each wallet
 , coinWallets        :: !Int
   -- ^ Number of wallets in use
 , coinWalletsSeed    :: !Int
   -- ^ Seed used to generate private keys for wallets
 , coinGeneratorDelay :: !(Maybe Int)
   -- ^ Delay between TX generation. Nothing means don't generate
 , coinMaxMempoolSize :: !Int
   -- ^ If mempool exceeds size new txs won't be generated
 , coinMaxBlockSize   :: !Int
   -- ^ Maximum number of transactions in block. Normally such things
   --   are hardcoded but we pass it as parameter for the sake of
   --   configurability.
 }
 deriving (Generic,Show)
 deriving JSON.FromJSON via HSChainCfg CoinSpecification


-- | Parameters for 'CoinT' monad transformer
data CoinDictM g = CoinDictM
  { dictGauges    :: !g
  , dictNamespace :: !Namespace
  , dictLogEnv    :: !LogEnv
  , dictConn      :: !(Connection 'RW)
  , dictCached    :: !(Cached BData)
  }
  deriving stock (Generic)

-- | Application monad for coin
newtype CoinT g m a = CoinT { unCoinT :: ReaderT (CoinDictM g) m a }
  deriving newtype (Functor,Applicative,Monad,MonadIO,MonadFail)
  deriving newtype (MonadThrow,MonadCatch,MonadMask,MonadFork)
  -- HSChain instances
  deriving MonadLogger
       via LoggerByFields "dictLogEnv" "dictNamespace" (ReaderT (CoinDictM g) m)
  deriving (MonadReadDB, MonadDB)
       via DatabaseByField "dictConn" (ReaderT (CoinDictM g) m)
  deriving (MonadCached BData)
       via CachedByField "dictCached" BData (ReaderT (CoinDictM g) m)

-- We have two variants of Monitoring depending on type parameter
deriving via NoMonitoring (CoinT () m)
    instance Monad m => MonadTMMonitoring (CoinT () m)
deriving via MonitoringByField "dictGauges" (ReaderT (CoinDictM PrometheusGauges) m)
    instance MonadIO m => MonadTMMonitoring (CoinT PrometheusGauges m)

instance MonadTrans (CoinT g) where
  lift = CoinT . lift

runCoinT :: CoinDictM g -> CoinT g m a -> m a
runCoinT d = flip runReaderT d . unCoinT

-- | Generate genesis from specification.
coinGenesis
  :: (Foldable f)
  => f (Validator (Alg BData))
  -> CoinSpecification
  -> Genesis BData
coinGenesis nodes CoinSpecification{..} = Genesis
  { genesisBlock  = genesis0
  , genesisValSet = valSet
  }
  where
    privK        = take coinWallets $ makePrivKeyStream coinWalletsSeed
    pubK         = publicKey <$> privK
    Right valSet = makeValidatorSet nodes
    txs          = [ Deposit pk coinAirdrop | pk <- pubK ]
    genesis0     = makeGenesis (BData txs) valSet valSet

-- | Interpret coin
interpretSpec
  :: (MonadDB m, MonadCached BData m, MonadFork m, MonadMask m, MonadLogger m, MonadTMMonitoring m)
  => Configuration Example      -- ^ Delays configuration
  -> BlockchainNet              -- ^ Network API
  -> NodeSpec BData             -- ^ Node specification
  -> [Validator (Alg BData)]    -- ^ List of validators
  -> CoinSpecification          -- ^ Specification
  -> AppCallbacks m BData       -- ^ Commit callbacks
  -> m (StateView m BData, [m ()])
interpretSpec cfg net NodeSpec{..} valSet coin@CoinSpecification{..} cb = do
  -- Start node
  -- (state,memThr,readST) <- inMemoryStateView $ genesisValSet genesis
  --
  -- We must ensure that database is initialized
  initDatabase
  mustQueryRW initCoinDB
  (state,memThr,stmt) <- databaseStateView coin $ genesisValSet genesis
  actions               <- runNode cfg NodeDescription
    { nodeValidationKey = nspecPrivKey
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb
    , nodeNetwork       = net
    , nodeStateView     = state
    }
  -- Allocate transactions generators
  txGenerator <- makeCoinGenerator coin >>= \case
    Nothing  -> return []
    Just txG -> do
      cursor <- getMempoolCursor $ mempoolHandle $ stateMempool state
      return [ dbTransactionGenerator stmt txG
                 (stateMempool state)
                 (pushTxAsync cursor)
             ]
  -- Done
  return
    ( state
    , txGenerator <> memThr <> actions
    )
  where
    genesis = coinGenesis valSet coin


-- | Execute node specification for mock network
executeNodeSpec
  :: ()
  => MockClusterConfig BData CoinSpecification
  -> AppCallbacks (CoinT () IO) BData
  -> ContT r IO [(StateView (CoinT () IO) BData, CoinDictM ())]
executeNodeSpec MockClusterConfig{..} callbacks = do
  -- Create mock network and allocate DB handles for nodes
  net       <- liftIO P2P.newMockNet
  resources <- allocNetwork net clusterTopology clusterNodes
  -- Start nodes
  rnodes    <- lift $ forM resources $ \(spec, bnet, dictConn, dictLogEnv) -> do
    dictCached <- newCached
    let dict = CoinDictM { dictGauges    = ()
                         , dictNamespace = mempty
                         , ..
                         }
    (state,threads) <- runCoinT dict $ interpretSpec
      clusterCfg
      bnet
      spec
      [ Validator (publicKey k) 1 | Just (PrivValidator k) <- nspecPrivKey <$> clusterNodes ]
      clusterBChData
      callbacks
    return (state, dict, threads)
  --
  lift $ catchAbort $ runConcurrently $ do (_,dict,threads) <- rnodes
                                           runCoinT dict <$> threads
  return [ (s,d) | (s,d,_) <- rnodes ]
