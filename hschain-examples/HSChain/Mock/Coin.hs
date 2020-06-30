{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
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
    -- ** Transaction generator
  , TxGenerator(..)
  , makeCoinGenerator
  , transactionGenerator
    -- * In-DB state
  , initCoinDB
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
import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Trans.Cont
import qualified Data.Aeson as JSON
import Data.Foldable
import Data.Either
import Data.IORef
import Data.Maybe
import Data.Map             (Map,(!))
import Database.SQLite.Simple             (Only(..))
import qualified Data.Vector         as V
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import Katip           (Namespace,LogEnv)
import System.Random   (randomRIO)
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
import HSChain.Mock.KeyList         (makePrivKeyStream)
import HSChain.Mock.Types
import HSChain.Monitoring
import qualified HSChain.Network.Mock as P2P
import qualified HSChain.Config


----------------------------------------------------------------
-- State management
----------------------------------------------------------------

-- | Context free TX validation. This function performs all checks
--   that could be done having onlyt
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
  => ValidatorSet (Alg BData)
  -> m (StateView m BData, [m ()], IO CoinState)
inMemoryStateView valSet0 = do
  varSt <- liftIO $ newIORef $ CoinState mempty mempty
  (mem@Mempool{..}, memThr) <- newMempool (isRight . validateTxContextFree)
  let make mh vals txList st = StateView
        { stateHeight       = mh
        , newValidators     = vals
        -- For commit we simply remove transaction in block from
        -- mempool and asking it to start filtering
        , commitState       = do
            removeTxByHashes $ hashed <$> txList
            startMempoolFiltering $ \tx -> return $ isRight $ processSend tx st
            liftIO $ writeIORef varSt st
        -- When we validate proposed block we want to do complete
        -- validation since block is sent from outside
        , validatePropBlock = \b valSet -> do
            let step s tx = do
                  validateTxContextFree tx
                  processTxFull (blockHeight b) tx s
            let txs = unBData $ merkleValue $ blockData b
                st' = foldM step st txs
            return $ make (Just $ blockHeight b) valSet txs <$> st'
        --
        , generateCandidate = \NewBlock{..} -> do
            memSt  <- getMempoolState
            let selectTx c []     = (c,[])
                selectTx c (t:tx) = case processSend t c of
                                      Left  _  -> selectTx c  tx
                                      Right c' -> let (c'', b  ) = selectTx c' tx
                                                  in  (c'', t:b)
            let (st', dat) = selectTx st
                           $ map merkleValue
                           $ toList
                           $ mempFIFO memSt
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
  { genPrivateKeys    :: V.Vector (PrivKey (Alg BData))
    -- ^ Private keys for which we can generate transactions
  , genDestinaions    :: V.Vector (PublicKey (Alg BData))
    -- ^ List of all addresses to which we can send money
  , genDelay          :: Int
    -- ^ Delay between invokations of generator
  , genMaxMempoolSize :: Int
  }

-- | Create generator for coin transactions
makeCoinGenerator
  :: CoinSpecification
  -> Maybe TxGenerator
makeCoinGenerator CoinSpecification{..} = do
  genDelay <- coinGeneratorDelay
  return TxGenerator
    { genPrivateKeys    = V.fromList privK
    , genDestinaions    = V.fromList pubK
    , genMaxMempoolSize = coinMaxMempoolSize
    , ..
    }
  where
    privK = take coinWallets $ makePrivKeyStream coinWalletsSeed
    pubK  = publicKey <$> privK

-- | Run generator for transactions
transactionGenerator
  :: MonadIO m
  => TxGenerator
  -> Mempool m (Alg BData) Tx
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
  privK  <- selectFromVec genPrivateKeys
  target <- selectFromVec genDestinaions
  amount <- randomRIO (1,20)
  let pubK      = publicKey privK
      allInputs = toList
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

selectFromVec :: V.Vector a -> IO a
selectFromVec v = do
  i <- randomRIO (0, V.length v - 1)
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

-- | Initialize tables for storage of coin state. Storage is heavily
--   tailored towards particular use case of demonstartion and
--   benchmarks for mock UTXO blockchain and likely won't work well
--   for more realistic case.
initCoinDB :: MonadQueryRW BData m => m ()
initCoinDB = do
  -- Table for unspent transactions. In order to be able to work with
  -- several instances of StateView we keep heights when UTXO was
  -- creted and spent
  basicExecute_
    "CREATE TABLE IF NOT EXISTS coin_utxo \
    \  ( tx_hash BLOB    NOT NULL \
    \  , n_out   INTEGER NOT NULL \
    \  , n_coins INTEGER NOT NULL \
    \  , pk      BLOB    NOT NULL \
    \  , h_added INTEGER NOT NULL \
    \  , h_spent INTEGER NULL     \
    \  , UNIQUE (tx_hash,n_out)   \
    \  , FOREIGN KEY (tx_hash) REFERENCES coin_pk(id))"


data UtxoChange
  = Spent !Height
    -- ^ Already existing UTXO is spent at given H
  | Added !Unspent !Height
    -- ^ UTXO was created at given H
  | Both  !Unspent !Height !Height
    -- ^ UTXO was created and then spent

-- | State difference
newtype UtxoDiff = UtxoDiff
  { utxoDiff :: Map UTXO UtxoChange
  }

-- | Lookup unspent output in database
dbLookupUTXO
  :: (MonadQueryRO BData m)
  => Height                     -- ^ Height at which state is calculated
  -> Hashed (Alg BData) Tx      -- ^ Transaction hash
  -> Int                        -- ^ Output number
  -> m (Maybe Unspent)
dbLookupUTXO h txHash nOut = do
  r <- basicQuery1
    "SELECT pk FROM coin_utxo \
    \ WHERE tx_hash = ? AND n_out = ? \
    \   AND (h_spent is NULL OR h_spent < ?"
    (encodeToBS txHash, nOut, h)
  return $ case r of
    Nothing     -> Nothing
    Just (bs,n) -> Just $ Unspent (fromMaybe (error "Invalid value in DB") $ decodeFromBS bs) n

dbProcessDeposit
  :: (Monad m)
  => Tx -> UtxoDiff -> ExceptT CoinError m UtxoDiff
dbProcessDeposit Send{} _ = throwError $ UnexpectedSend
dbProcessDeposit tx@(Deposit pk nCoin) (UtxoDiff m) =
  return $ UtxoDiff $ Map.insert utxo (Added (Unspent pk nCoin) (Height 0)) m
  where
    utxo = UTXO 0 (hashed tx)

dbProcessSend
  :: (MonadQueryRO BData m)
  => Height -> Tx -> UtxoDiff -> ExceptT CoinError m UtxoDiff
dbProcessSend _ Deposit{} _ = throwError DepositAtWrongH
dbProcessSend h tx@(Send pk _ TxSend{..}) diff = do
  -- Try to find all inputs for transaction
  inputs <- forM txInputs $ \utxo@(UTXO nOut txH) -> do
    Unspent pk' n <- case utxo `Map.lookup` utxoDiff diff of
      Just (Added u _) -> return u
      Just _               -> throwError $ CoinError "Already spent output"
      Nothing -> dbLookupUTXO h txH nOut >>= \case
        Nothing -> throwError $ CoinError "Already spent output"
        Just x  -> return x
    unless (pk == pk') $ throwError $ CoinError "PublicKey mismatch"
    return n
  -- Check that sum of inputs and outputs match
  unless (sum inputs == sum [n | Unspent _ n <- txOutputs])
    $ throwError $ CoinError "Missmatch between inputs and outputs"
  -- Update diff
  return
    $ UtxoDiff
    $ (\m -> foldl' addOutput  m ([0..] `zip` txOutputs))
    $ (\m -> foldl' spendInput m txInputs)    
    $ utxoDiff diff
  where
    txHash = hashed tx
    spendInput m utxo = Map.alter
      (\case
          Nothing           -> Just $ Spent h
          Just (Added u h0) -> Just $ Both u h0 h
          Just _            -> error "Coin: internal error"
      ) utxo m
    addOutput m (i,u) = Map.insert (UTXO i txHash) (Added u h) m



databaseStateView
  :: (MonadIO m, MonadDB BData m)
  => m (StateView m BData, [m ()])
databaseStateView = do
  (mem@Mempool{..}, memThr) <- newMempool (isRight . validateTxContextFree)
  -- First we find what is latest height at which we updated state and
  -- use it as startign point for our state management.
  (h0,valSet0) <- queryRO $ do
    h <- fmap fromOnly <$> basicQuery1 "SELECT MAX(h_added) FROM coin_utxo" ()
    v <- mustRetrieveValidatorSet $ succH h
    return (h,v)
  -- Create state view
  let make mh vals diff = StateView
        { stateHeight       = mh
        , newValidators     = vals
        , commitState       = undefined
        , validatePropBlock = \b valSet -> do
            let step d tx
                  | h == Height 0 = dbProcessDeposit tx d
                  | otherwise     = dbProcessSend h tx d
                  where
                    h = blockHeight b
            diff' <- queryRO $ runExceptT $ foldM step diff $ unBData $ merkleValue $ blockData b
            undefined
        , generateCandidate = \NewBlock{..} -> do
            undefined
        , stateMempool      = mem
        }
  -- Read 
  return ( make undefined valSet0 (UtxoDiff Map.empty)
         , [memThr]
         )
  where
    succH = maybe (Height 0) succ



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
 }
 deriving (Generic,Show)
 deriving JSON.FromJSON via HSChainCfg CoinSpecification


-- | Parameters for 'CoinT' monad transformer
data CoinDictM g = CoinDictM
  { dictGauges    :: !g
  , dictNamespace :: !Namespace
  , dictLogEnv    :: !LogEnv
  , dictConn      :: !(Connection 'RW BData)
  }
  deriving stock (Generic)

-- | Application monad for coin
newtype CoinT g m a = CoinT { unCoinT :: ReaderT (CoinDictM g) m a }
  deriving newtype (Functor,Applicative,Monad,MonadIO)
  deriving newtype (MonadThrow,MonadCatch,MonadMask,MonadFork)
  deriving newtype (MonadReader (CoinDictM g))
  -- HSChain instances
  deriving MonadLogger
       via LoggerByFields "dictLogEnv" "dictNamespace" (CoinT g m)
  deriving (MonadReadDB BData, MonadDB BData)
       via DatabaseByField "dictConn" BData (CoinT g m)

-- We have two variants of Monitoring depending on type parameter
deriving via NoMonitoring (CoinT () m)
    instance Monad m => MonadTMMonitoring (CoinT () m)
deriving via MonitoringByField "dictGauges" (CoinT PrometheusGauges m)
    instance MonadIO m => MonadTMMonitoring (CoinT PrometheusGauges m)

runCoinT :: CoinDictM g -> CoinT g m a -> m a
runCoinT d
  = flip runReaderT d . unCoinT

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
  :: (MonadDB BData m, MonadFork m, MonadMask m, MonadLogger m, MonadTMMonitoring m)
  => Configuration Example      -- ^ Delays configuration
  -> BlockchainNet              -- ^ Network API
  -> NodeSpec BData             -- ^ Node specification
  -> [Validator (Alg BData)]    -- ^ List of validators
  -> CoinSpecification          -- ^ Specification
  -> AppCallbacks m BData       -- ^ Commit callbacks
  -> m (StateView m BData, [m ()])
interpretSpec cfg net NodeSpec{..} valSet coin@CoinSpecification{..} cb = do
  -- Start node
  (state,memThr,readST) <- inMemoryStateView $ genesisValSet genesis
  actions               <- runNode cfg NodeDescription
    { nodeValidationKey = nspecPrivKey
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb
    , nodeNetwork       = net
    , nodeStateView     = state
    }
  -- Allocate transactions generators
  txGenerator <- case makeCoinGenerator coin of
    Nothing  -> return []
    Just txG -> do
      cursor <- getMempoolCursor $ mempoolHandle $ stateMempool state
      return [ transactionGenerator txG
                 (stateMempool state)
                 (liftIO readST)
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
