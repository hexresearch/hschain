{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
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
  , coinLogic
    -- * Transaction generator
  , mintMockCoin
  , generateTransaction
  , transactionGenerator
  , TxGenerator(..)
    -- * Interpretation
  , interpretSpec
  , RunningNode(..)
  , executeNodeSpec
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Concurrent   (threadDelay)
import Control.DeepSeq
import Codec.Serialise      (Serialise)
import qualified Data.Aeson as JSON
import Data.Foldable
import Data.Maybe
import Data.Map             (Map,(!))
import qualified Data.Vector         as V
import qualified Data.Map.Strict     as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set
import System.Random   (randomRIO)
import GHC.Generics    (Generic)

import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
import HSChain.Types.Validators
import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.Debug.Trace
import HSChain.Logger
import HSChain.Run
import HSChain.Mock
import HSChain.Store
import HSChain.Store.STM
import HSChain.Mock.KeyList         (makePrivKeyStream)
import HSChain.Mock.Types
import HSChain.Monitoring
import qualified HSChain.P2P.Network as P2P


----------------------------------------------------------------
-- Basic coin logic
----------------------------------------------------------------

-- | Block data. It's simply newtype wrapper over list of
--   transactions. Newtype is needed in order to define 'BlockData'
--   instance.
newtype BData = BData [Tx]
  deriving stock    (Show,Eq,Generic)
  deriving newtype  (NFData,CryptoHashable,JSON.ToJSON,JSON.FromJSON)
  deriving anyclass (Serialise)

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
  type Alg             BData = Ed25519 :& SHA512
  bchLogic                      = coinLogic
  proposerSelection             = ProposerSelection randomProposerSHA512
  blockTransactions (BData txs) = txs
  logBlockData      (BData txs) = HM.singleton "Ntx" $ JSON.toJSON $ length txs

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

-- | Complete description of blockchain logic. Since we keep all state
--   in memory we simply use @Maybe@ to track failures
coinLogic :: BChLogic (Either CoinError) BData
coinLogic = BChLogic
  { processTx     = \BChEval{..} -> void $ processTransaction bchValue (merkleValue blockchainState)
  --
  , processBlock  = \BChEval{..} -> do
      let h    = blockHeight bchValue
          step = flip $ process h
      st <- foldM step (merkleValue blockchainState) $ blockTransactions $ merkleValue $ blockData bchValue
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


mintMockCoin
  :: (Foldable f)
  => f (Validator (Alg BData))
  -> CoinSpecification
  -> (Maybe TxGenerator, Genesis BData)
mintMockCoin nodes CoinSpecification{..} =
  ( do delay <- coinGeneratorDelay
       return TxGenerator
         { genPrivateKeys    = V.fromList privK
         , genDestinaions    = V.fromList pubK
         , genDelay          = delay
         , genMaxMempoolSize = coinMaxMempoolSize
         }
  , BChEval
    { bchValue        = genesis0 { blockStateHash = merkleHashed st }
    , validatorSet    = merkled valSet
    , blockchainState = merkled state0
    }
  )
  where
    privK        = take coinWallets $ makePrivKeyStream coinWalletsSeed
    pubK         = publicKey <$> privK
    Right valSet = makeValidatorSet nodes
    txs          = [ Deposit pk coinAridrop | pk <- pubK ]
    -- Generate genesis with correct hash
    state0       = CoinState mempty mempty
    genesis0     = makeGenesis (BData txs) (Hashed $ hash ()) valSet valSet
    Right BChEval{blockchainState=st}
      = processBlock coinLogic BChEval
        { bchValue        = genesis0
        , validatorSet    = merkled valSet
        , blockchainState = merkled state0
        }

findInputs :: (Num i, Ord i) => i -> [(a,i)] -> [(a,i)]
findInputs tgt = go 0
  where go _ [] = []
        go acc ((tx,i):rest)
          | acc' >= tgt = [(tx,i)]
          | otherwise   = (tx,i) : go acc' rest
          where
            acc' = acc + i


----------------------------------------------------------------
-- Interpretation of coin
----------------------------------------------------------------

interpretSpec
  :: ( MonadDB m BData, MonadFork m, MonadMask m, MonadLogger m
     , MonadTrace m, MonadTMMonitoring m
     , Has x BlockchainNet
     , Has x NodeSpec
     , Has x (Configuration Example))
  => Genesis BData
  -> x
  -> AppCallbacks m BData
  -> m (RunningNode m BData, [m ()])
interpretSpec genesis p cb = do
  conn    <- askConnectionRO
  store   <- newSTMBchStorage $ blockchainState genesis
  mempool <- makeMempool store (ExceptT . return)
  acts <- runNode (getT p :: Configuration Example) NodeDescription
    { nodeValidationKey = p ^.. nspecPrivKey
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb <> nonemptyMempoolCallback mempool
    , nodeRunner        = ExceptT . return
    , nodeStore         = AppStore { appBchState = store
                                   , appMempool  = mempool
                                   }
    , nodeNetwork       = getT p
    }
  return
    ( RunningNode { rnodeState   = store
                  , rnodeConn    = conn
                  , rnodeMempool = mempool
                  }
    , acts
    )

executeNodeSpec
  :: (MonadIO m, MonadMask m, MonadFork m, MonadTrace m, MonadTMMonitoring m)
  => NetSpec NodeSpec :*: CoinSpecification
  -> ContT r m [RunningNode m BData]
executeNodeSpec (NetSpec{..} :*: coin@CoinSpecification{..}) = do
  -- Create mock network and allocate DB handles for nodes
  net       <- liftIO P2P.newMockNet
  resources <- traverse (\x -> do { r <- allocNode x; return (x,r)})
             $ allocateMockNetAddrs net netTopology
             $ netNodeList
  -- Start nodes
  rnodes    <- lift $ forM resources $ \(x, (conn, logenv)) -> do
    let run :: DBT 'RW BData (LoggerT m) x -> m x
        run = runLoggerT logenv . runDBT conn
    (rn, acts) <- run $ interpretSpec genesis (netNetCfg :*: x)
      (maybe mempty callbackAbortAtH netMaxH)
    return ( hoistRunningNode run rn
           , run <$> acts
           )
  -- Allocate transactions generators
  txGens <- lift $ case mtxGen of
    Nothing  -> return []
    Just txG -> forM rnodes $ \(RunningNode{..}, _) -> do
      cursor <- getMempoolCursor rnodeMempool
      return $ transactionGenerator txG
        rnodeMempool
        (merkleValue . snd <$> bchCurrentState rnodeState)
        (void . pushTransaction cursor)
  -- Actually run nodes
  lift   $ catchAbort $ runConcurrently $ (snd =<< rnodes) ++ txGens
  return $ fst <$> rnodes
  where
    (mtxGen, genesis) = mintMockCoin
      [ Validator (publicKey k) 1
      | Just (PrivValidator k) <- nspecPrivKey <$> netNodeList
      ] coin
