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
import Data.Foldable
import Data.Maybe
import Data.Map             ((!))
import qualified Data.Vector         as V
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import System.Random   (randomRIO)

import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
import HSChain.Types.Validators
import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control.Class
import HSChain.Crypto
import HSChain.Logger
import HSChain.Run
import HSChain.Mock
import HSChain.Mock.Coin.Types
import HSChain.Store
import HSChain.Store.STM
import HSChain.Mock.KeyList         (makePrivKeyStream)
import HSChain.Mock.Types
import HSChain.Monitoring
import qualified HSChain.Network.Mock as P2P


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
    txs          = [ Deposit pk coinAirdrop | pk <- pubK ]
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
  :: (MonadDB BData m, MonadFork m, MonadMask m, MonadLogger m, MonadTMMonitoring m)
  => Genesis BData
  -> Configuration Example
  -> BlockchainNet
  -> NodeSpec BData
  -> AppCallbacks m BData
  -> m (RunningNode m BData, [m ()])
interpretSpec genesis cfg net spec cb = do
  conn    <- askConnectionRO
  store   <- maybe return snapshotState (nspecPersistIval spec)
         =<< newSTMBchStorage (blockchainState genesis)
  mempool <- makeMempool store (ExceptT . return)
  acts <- runNode cfg NodeDescription
    { nodeValidationKey = nspecPrivKey spec
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb <> nonemptyMempoolCallback mempool
    , nodeRunner        = ExceptT . return
    , nodeStore         = AppStore { appBchState = store
                                   , appMempool  = mempool
                                   }
    , nodeNetwork       = net
    }
  return
    ( RunningNode { rnodeState   = store
                  , rnodeConn    = conn
                  , rnodeMempool = mempool
                  }
    , acts
    )

executeNodeSpec
  :: (MonadIO m, MonadMask m, MonadFork m, MonadTMMonitoring m)
  => NetSpec (NodeSpec BData)
  -> CoinSpecification
  -> ContT r m [RunningNode m BData]
executeNodeSpec NetSpec{..} coin@CoinSpecification{..} = do
  -- Create mock network and allocate DB handles for nodes
  net       <- liftIO P2P.newMockNet
  resources <- allocNetwork net netTopology netNodeList  
  -- Start nodes
  rnodes    <- lift $ forM resources $ \(spec, bnet, conn, logenv) -> do
    let run :: DBT 'RW BData (LoggerT m) x -> m x
        run = runLoggerT logenv . runDBT conn
    (rn, acts) <- run $ interpretSpec
      genesis
      netNetCfg
      bnet
      spec
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
