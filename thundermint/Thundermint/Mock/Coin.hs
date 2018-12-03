{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
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
  , unspentOutputsLens
  , coinDict
    -- * Transaction generator
  , GeneratorSpec(..)
  , defaultGenerator
  , restrictGenerator
  , genesisFromGenerator
    -- ** Generator
  , generateTransaction
  , transactionGenerator
    -- ** interpretation
  , interpretSpec
  , executeNodeSpec
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Concurrent   (threadDelay)
import Control.DeepSeq
import Codec.Serialise      (Serialise,serialise)
import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.Functor.Compose
import Data.Int
import Data.Map             (Map)
import qualified Data.Map.Strict  as Map
import Lens.Micro
import System.Random   (randomRIO)
import GHC.Generics    (Generic)

import Thundermint.Blockchain.Types
import Thundermint.Blockchain.Interpretation
import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Containers (ValidatorSet)
import Thundermint.Crypto.Ed25519
import Thundermint.Debug.Trace
import Thundermint.Logger
import Thundermint.Run
import Thundermint.Store.SQL
import Thundermint.Store
import Thundermint.Store.Internal.Types
import Thundermint.Store.Internal.Query (connectionRO)
import Thundermint.Mock.KeyList (privateKeyList)
import Thundermint.Mock.Types
import Thundermint.Monitoring
import qualified Thundermint.P2P.Network as P2P


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

instance FunctorF CoinStateDB where
  fmapF f (CoinStateDB u) = CoinStateDB (f u)

instance FloatOut CoinStateDB where
  floatOut (CoinStateDB (Compose u)) = fmap CoinStateDB u
  traverseEff f (CoinStateDB u) = f u

transitionsDB :: PersistentState CoinStateDB Alg [Tx]
transitionsDB = PersistentState
  { processTxDB           = \_ -> processTransactionDB
  , processBlockDB        = \b -> forM_ (blockData b) $ processDB (headerHeight (blockHeader b))
  , transactionsToBlockDB = \h ->
      let selectTx []     = return []
          selectTx (t:tx) = optional (processDB h t) >>= \case
            Nothing -> selectTx tx
            Just () -> (t :) <$> selectTx tx
      in selectTx
  -- DB schema
  , persistedData = coinDict
  }

coinDict :: CoinStateDB Persistent
coinDict = CoinStateDB
  { unspentOutputsDB = wrap $ PMap { pmapTableName = "utxo"
                                   , pmapEncodingK = encodingCBOR
                                   , pmapEncodingV = encodingCBOR
                                   }
  }

processDB
  :: (Monad (q CoinStateDB), ExecutorRW q)
  => Height
  -> Tx
  -> q CoinStateDB ()
processDB (Height 0) (Deposit pk nCoin) = processDepositDB pk nCoin
processDB _          tx                 = processTransactionDB tx

processDepositDB :: (Monad (q CoinStateDB), ExecutorRW q)
                 => PublicKey Alg -> Integer -> q CoinStateDB ()
processDepositDB pk nCoin = do
  storeKey unspentOutputsLens (hash (Deposit pk nCoin),0) (pk,nCoin)



processTransactionDB
  :: (Monad (q CoinStateDB), ExecutorRW q)
  => Tx
  -> q CoinStateDB ()
processTransactionDB Deposit{} = fail ""
processTransactionDB transaction@(Send pubK sig txSend@TxSend{..}) = do
  -- Signature must be valid
  check $ verifyCborSignature pubK txSend sig
  -- Inputs and outputs are not null
  check $ not $ null txInputs
  check $ not $ null txOutputs
  -- Outputs are all positive
  forM_ txOutputs $ \(_,n) -> check (n > 0)
  -- Inputs are owned Spend and generated amount match and transaction
  -- issuer have rights to funds
  inputs <- forM txInputs $ \i -> do
    Just (pk,n) <- lookupKey unspentOutputsLens i
    check $ pk == pubK
    return n
  check (sum inputs == sum (map snd txOutputs))
  -- Update application state
  let txHash = hashBlob $ toStrict $ serialise transaction
  forM_ txInputs  $ dropKey  unspentOutputsLens
  forM_ ([0..] `zip` txOutputs) $ \(i,out) ->
    storeKey unspentOutputsLens (txHash,i) out
  where
    check True  = return ()
    check False = fail ""

unspentOutputsLens :: Lens' (CoinStateDB f) (f (PMap (Hash Alg, Int) (PublicKey Alg, Integer)))
unspentOutputsLens = lens unspentOutputsDB (const CoinStateDB)


----------------------------------------------------------------
-- Transaction generator
----------------------------------------------------------------

-- | Specification for transaction generator
data GeneratorSpec = GeneratorSpec
  { genInitialKeys    :: [PublicKey Alg] -- ^ Public keys of all wallets
  , genPrivateKeys    :: [PrivKey   Alg]
  , genInitialDeposit :: Integer
  , genDelay          :: Int
  }
  deriving (Show)

-- | Default generator which uses 'privateKeyList' as source of
--   keys. Useful since it allows to specify generator concisely.
defaultGenerator :: Int -> Integer -> Int ->  GeneratorSpec
defaultGenerator n dep delay = GeneratorSpec
  { genInitialKeys    = map publicKey pk
  , genPrivateKeys    = pk
  , genInitialDeposit = dep
  , genDelay          = delay
  }
  where pk = take n privateKeyList

-- | @restrictGenerator i n@ restrict generator to only generate
--   transaction for ith nth of all private keys
restrictGenerator :: Int -> Int -> GeneratorSpec -> GeneratorSpec
restrictGenerator n tot _
  | n < 0 || tot < 0 || n >= tot = error "restrictGenerator: invalid parameters"
restrictGenerator n tot GeneratorSpec{..} = GeneratorSpec
  { genPrivateKeys = take (off2-off1) $ drop off1 genPrivateKeys
  , ..
  }
  where
    len  = length genPrivateKeys
    off1 = (n     * len) `div` tot
    off2 = ((n+1) * len) `div` tot

genesisFromGenerator :: ValidatorSet Alg -> GeneratorSpec -> Block Alg [Tx]
genesisFromGenerator validatorSet GeneratorSpec{..} = Block
  { blockHeader = Header
      { headerChainID        = "MONIES"
      , headerHeight         = Height 0
      , headerTime           = Time 0
      , headerLastBlockID    = Nothing
      , headerValidatorsHash = hash validatorSet
      , headerDataHash       = hash dat
      }
  , blockData       = dat
  , blockLastCommit = Nothing
  , blockEvidence   = []
  }
  where
    dat = [ Deposit pk genInitialDeposit | pk <- genInitialKeys ]

-- | Generate transaction. This implementation is really inefficient
--   since it will materialize all unspent outputs into memory and
--   shouldn't be used when number of wallets and coins is high
generateTransaction :: GeneratorSpec -> IO (EphemeralQ Alg [Tx] CoinStateDB Tx)
generateTransaction GeneratorSpec{..} = do
  -- Pick private key
  privK <- do i <- randomRIO (0, length genPrivateKeys - 1)
              return (genPrivateKeys !! i)
  let pubK = publicKey privK
  -- Pick public key to send data to
  target <- do i <- randomRIO (0, nPK - 1)
               return (genInitialKeys !! i)
  amount <- randomRIO (0,20)
  -- Create transaction
  return $ do
    utxo <- materializePMap unspentOutputsLens
    let inputs = findInputs amount [ (inp, n)
                                   | (inp, (pk,n)) <- Map.toList utxo
                                   , pk == pubK
                                   ]
        tx     = TxSend { txInputs  = map fst inputs
                        , txOutputs = [ (target, amount)
                                      , (pubK  , sum (map snd inputs) - amount)
                                      ]
                        }
    return $ Send pubK (signBlob privK $ toStrict $ serialise tx) tx
  where
    nPK  = length genInitialKeys


-- | Generate transaction indefinitely
transactionGenerator
  :: (MonadIO m, MonadDB m Alg [Tx])
  => GeneratorSpec -> (Tx -> m ()) -> m ()
transactionGenerator gen push = forever $ do
  txGen   <- liftIO $ generateTransaction gen
  Just tx <- queryRO
           $ runEphemeralQ coinDict
           $ txGen
  push tx
  liftIO $ threadDelay (genDelay gen * 1000)


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

-- | Interpret specification for node
interpretSpec
  :: ( MonadIO m, MonadLogger m, MonadFork m, MonadTrace m, MonadMask m
     , Ord addr, Show addr, Serialise addr)
  => Maybe Int64                      -- ^ Maximum height
  -> GeneratorSpec                    -- ^ Spec for generator of transactions
  -> ValidatorSet Ed25519_SHA512      -- ^ Set of validators
  -> BlockchainNet addr               -- ^ Network
  -> NodeSpec                         -- ^ Node specifications
  -> m (Connection 'RO Alg [Tx], m ())
interpretSpec maxHeight genSpec validatorSet net NodeSpec{..} = do
  -- Allocate storage for node
  conn <- openConnection (maybe ":memory:" id nspecDbName)
  initDatabase conn coinDict genesisBlock validatorSet
  return
    ( connectionRO conn
    , runDBT conn $ do
        logic  <- logicFromPersistent transitionsDB
        -- Transactions generator
        cursor <- getMempoolCursor $ nodeMempool logic
        let generator = transactionGenerator genSpec (void . pushTransaction cursor)
        acts <- runNode (defCfg :: Configuration Example) net
          NodeDescription
            { nodeValidationKey   = nspecPrivKey
            , nodeCommitCallback  = \case
                b | Just hM <- maxHeight
                  , headerHeight (blockHeader b) > Height hM -> throwM Abort
                  | otherwise                                -> return ()
            , nodeMonitoring      = noMonitoring
            }
          logic
        runConcurrently (generator : acts)
    )
  where
    genesisBlock :: Block Alg [Tx]
    genesisBlock = genesisFromGenerator validatorSet genSpec

--
executeNodeSpec
  :: Maybe Int64                -- ^ Maximum height
  -> Int                        -- ^ Delay for generator
  -> NetSpec NodeSpec           -- ^ Specification for net
  -> IO [Connection 'RO Alg [Tx]]
executeNodeSpec maxH delay NetSpec{..} = do
  net <- P2P.newMockNet
  let totalNodes   = length netNodeList
      netAddresses = Map.fromList $ [0::Int ..] `zip` netNodeList
      connections  = case netTopology of
        Ring    -> connectRing
        All2All -> connectAll2All
      validatorSet = makeValidatorSetFromPriv [ pk | Just pk <- nspecPrivKey <$> netNodeList ]

  actions <- forM (Map.toList netAddresses) $ \(addr, nspec@NodeSpec{..}) -> do
    let genSpec = restrictGenerator addr totalNodes
                $ defaultGenerator netInitialKeys netInitialDeposit delay
        bnet    = BlockchainNet
          { bchNetwork      = P2P.createMockNode net "50000" addr
          , bchLocalAddr    = (addr, "50000")
          , bchInitialPeers = map (,"50000") $ connections netAddresses addr
          }
    let loggers = [ makeScribe s | s <- nspecLogFile ]
        run m   = withLogEnv "TM" "DEV" loggers $ \logenv -> runLoggerT "general" logenv m
    run $ (fmap . fmap) run $ interpretSpec maxH genSpec validatorSet bnet nspec
  runConcurrently (snd <$> actions) `catch` (\Abort -> return ())
  return $ fst <$> actions
