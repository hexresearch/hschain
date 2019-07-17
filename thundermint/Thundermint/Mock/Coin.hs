{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
-- |
-- Simple coin for experimenting with blockchain
module Thundermint.Mock.Coin (
    Alg
  , TxSend(..)
  , Tx(..)
    -- * Pure state
  , CoinState(..)
  , transitions
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

import Control.Applicative
import Control.Monad

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Concurrent   (threadDelay)
import Control.DeepSeq
import Codec.Serialise      (Serialise,serialise)
import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.Map             (Map)
import qualified Data.Vector      as V
import qualified Data.Map.Strict  as Map
import System.Random   (randomRIO)
import GHC.Generics    (Generic)

import Thundermint.Types.Blockchain
import Thundermint.Types.Validators
import Thundermint.Blockchain.Interpretation
import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Ed25519
import Thundermint.Crypto.SHA
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Run
import Thundermint.Store
import Thundermint.Mock.KeyList         (makePrivKeyStream)
import Thundermint.Mock.Types
import Thundermint.Monitoring
import qualified Thundermint.P2P.Network as P2P


----------------------------------------------------------------
-- Basic coin logic
----------------------------------------------------------------

type Alg = (Ed25519 :& SHA512)

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
  deriving (Show, NFData, Generic, Serialise)


processDeposit :: Tx -> CoinState -> Maybe CoinState
processDeposit Send{}                _             = Nothing
processDeposit tx@(Deposit pk nCoin) CoinState{..} =
  return CoinState
    { unspentOutputs = Map.insert (hash tx,0) (pk,nCoin) unspentOutputs
    }


processTransaction :: Tx -> CoinState -> Maybe CoinState
processTransaction Deposit{} _ = Nothing
processTransaction transaction@(Send pubK sig txSend@TxSend{..}) CoinState{..} = do
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
  -- Signature must be valid. Note signature check is expensive so
  -- it's done at last moment
  guard $ verifyCborSignature pubK txSend sig
  return CoinState
    { unspentOutputs =
        let spend txMap = foldl' (flip  Map.delete) txMap txInputs
            add   txMap = foldl'
                            (\m (i,out) -> Map.insert (txHash,i) out m)
                            txMap ([0..] `zip` txOutputs)
        in add $ spend unspentOutputs
    }

transitions :: BlockFold CoinState alg [Tx]
transitions = BlockFold
  { processTx           = const process
  , processBlock        = \_ b s0 -> let h = headerHeight $ blockHeader b
                                   in foldM (flip (process h)) s0 (blockData b)
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
-- Transaction generator
----------------------------------------------------------------

-- | Specification of generator of transactions
data TxGenerator = TxGenerator
  { genPrivateKeys    :: V.Vector (PrivKey Alg)
    -- ^ Private keys for which we can generate transactions
  , genDestinaions    :: V.Vector (PublicKey Alg)
    -- ^ List of all addresses to which we can send money
  , genDelay          :: Int
    -- ^ Delay between invokations of generator
  , genMaxMempoolSize :: Int
  }

transactionGenerator
  :: MonadIO m
  => TxGenerator
  -> Mempool m Alg Tx
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
generateTransaction TxGenerator{..} (CoinState utxo) = liftIO $ do
  privK  <- selectFromVec genPrivateKeys
  target <- selectFromVec genDestinaions
  amount <- randomRIO (0,20)
  let pubK   = publicKey privK
      inputs = findInputs amount [ (inp, n)
                                 | (inp, (pk,n)) <- Map.toList utxo
                                 , pk == pubK
                                 ]
      tx     = TxSend { txInputs  = map fst inputs
                      , txOutputs = [ (target, amount)
                                    , (pubK  , sum (snd <$> inputs) - amount)
                                    ]
                      }
  return $ Send pubK (signBlob privK $ toStrict $ serialise tx) tx

selectFromVec :: V.Vector a -> IO a
selectFromVec v = do
  i <- randomRIO (0, V.length v - 1)
  return $ v V.! i


mintMockCoin
  :: (Foldable f)
  => f (Validator Alg)
  -> CoinSpecification
  -> (Maybe TxGenerator, Block Alg [Tx])
mintMockCoin nodes CoinSpecification{..} =
  ( do delay <- coinGeneratorDelay
       return TxGenerator
         { genPrivateKeys    = V.fromList privK
         , genDestinaions    = V.fromList pubK
         , genDelay          = delay
         , genMaxMempoolSize = coinMaxMempoolSize
         }
  , makeGenesis "MONIES" (Time 0) txs valSet
  )
  where
    privK        = take coinWallets $ makePrivKeyStream coinWalletsSeed
    pubK         = publicKey <$> privK
    Right valSet = makeValidatorSet nodes
    txs          = [ Deposit pk coinAridrop | pk <- pubK ]


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
  :: ( MonadDB m Alg [Tx], MonadFork m, MonadMask m, MonadLogger m
     , MonadTrace m, MonadTMMonitoring m
     , Has x BlockchainNet
     , Has x NodeSpec
     , Has x (Configuration Example))
  => x
  -> AppCallbacks m Alg [Tx]
  -> m (RunningNode CoinState m Alg [Tx], [m ()])
interpretSpec p cb = do
  conn              <- askConnectionRO
  (bchState, logic) <- logicFromFold transitions
  acts <- runNode (getT p :: Configuration Example) NodeDescription
    { nodeValidationKey = p ^.. nspecPrivKey
    , nodeCallbacks     = cb <> nonemptyMempoolCallback (appMempool logic)
    , nodeLogic         = logic
    , nodeNetwork       = getT p
    }
  return
    ( RunningNode { rnodeState   = bchState
                  , rnodeConn    = conn
                  , rnodeMempool = appMempool logic
                  }
    , acts
    )

  
executeNodeSpec
  :: (MonadIO m, MonadMask m, MonadFork m, MonadTrace m, MonadTMMonitoring m)
  => NetSpec NodeSpec :*: CoinSpecification
  -> ContT r m [RunningNode CoinState m Alg [Tx]]
executeNodeSpec (NetSpec{..} :*: coin@CoinSpecification{..}) = do
  -- Create mock network and allocate DB handles for nodes
  net       <- liftIO P2P.newMockNet
  resources <- traverse (allocNode genesis)
             $ allocateMockNetAddrs net netTopology
             $ netNodeList
  -- Start nodes
  rnodes    <- lift $ forM resources $ \(x, conn, logenv) -> do
    let run = runLoggerT logenv . runDBT conn
    (rn, acts) <- run $ interpretSpec (netNetCfg :*: x)
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
        (currentState rnodeState)
        (void . pushTransaction cursor)
  -- Actually run nodes
  lift   $ catchAbort $ runConcurrently $ (snd =<< rnodes) ++ txGens
  return $ fst <$> rnodes
  where
    (mtxGen, genesis) = mintMockCoin [ Validator (publicKey k) 1
                                     | Just (PrivValidator k) <- nspecPrivKey <$> netNodeList
                                     ] coin
