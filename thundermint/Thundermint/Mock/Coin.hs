{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
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

  , intToNetAddr
  ) where

import Prelude hiding (fail)
import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Concurrent   (threadDelay)
import Control.DeepSeq
import Codec.Serialise      (Serialise,serialise)
import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.SafeCopy
import Data.Functor.Compose
import Data.Int
import Data.Map             (Map)
import qualified Data.Map.Strict  as Map
import Lens.Micro
import System.Random   (randomRIO)
import GHC.Generics    (Generic)

import Thundermint.P2P
import Thundermint.Types.Blockchain
import Thundermint.Blockchain.Interpretation
import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Control
import Thundermint.Crypto
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
import Thundermint.Types.Validators (ValidatorSet)
import qualified Thundermint.P2P.Network as P2P


type Alg = Ed25519_SHA512

-- | Single transaction for transfer of coins
data TxSend = TxSend
  { txInputs  :: [(Hash Alg, Int)]
  , txOutputs :: [(PublicKey Alg, Integer)]
  }
  deriving (Show, Eq, Ord, Generic)
instance SafeCopy  TxSend
instance NFData    TxSend
instance JSON.ToJSON   TxSend
instance JSON.FromJSON TxSend

data Tx
  = Deposit !(PublicKey Alg) !Integer
    -- ^ Deposit tokens to given key. Could only appear in genesis
    --   block
  | Send !(PublicKey Alg) !(Signature Alg) !(Pet TxSend)
    -- ^ Send coins to other addresses. Transaction must obey
    --   following invariants:
    --
    --   0. Signature must be valid
    --   1. All inputs must be owned by transaction issuer
    --   3. Inputs and outputs must be nonempty
    --   2. Sum of inputs must be equal to sum of outputs
  deriving (Show, Eq, Ord, Generic)
instance SafeCopy  Tx
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

processDeposit :: Pet Tx -> CoinState -> Maybe CoinState
processDeposit tx CoinState{..} = case pet tx of
  Send{}           -> Nothing
  Deposit pk nCoin -> return CoinState
    { unspentOutputs = Map.insert (hash tx,0) (pk,nCoin) unspentOutputs
    }


processTransaction :: Pet Tx -> CoinState -> Maybe CoinState
processTransaction tx CoinState{..} = case pet tx of
  Deposit{} -> Nothing
  Send pubK sig txSend@(pet -> TxSend{..}) -> do
    -- Signature must be valid
    guard $ verifyPetrifiedSignature pubK txSend sig
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
    let txHash = hash tx
    return CoinState
      { unspentOutputs =
          let spend txMap = foldl' (flip  Map.delete) txMap txInputs
              add   txMap = foldl'
                              (\m (i,out) -> Map.insert (txHash,i) out m)
                              txMap ([0..] `zip` txOutputs)
          in add $ spend unspentOutputs
      }

transitions :: BlockFold CoinState alg [Pet Tx]
transitions = BlockFold
  { processTx           = process
  , processBlock        = \b s0 -> let h = headerHeight $ pet $ blockHeader b
                                   in foldM (flip (process h)) s0 (pet (blockData b))
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

transitionsDB :: PersistentState CoinStateDB Alg [Pet Tx]
transitionsDB = PersistentState
  { processTxDB           = \_ -> processTransactionDB
  , processBlockDB        = \b -> forM_ (pet (blockData b)) $ processDB (headerHeight (pet (blockHeader b)))
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
  :: (Monad (q CoinStateDB), ExecutorRW q, MonadFail (q CoinStateDB))
  => Height
  -> Pet Tx
  -> q CoinStateDB ()
processDB (Height 0) tx@(pet -> Deposit pk nCoin) = processDepositDB (hash tx) pk nCoin
processDB _          tx                           = processTransactionDB tx

processDepositDB :: (Monad (q CoinStateDB), ExecutorRW q)
                 => Hash Alg -> PublicKey Alg -> Integer -> q CoinStateDB ()
processDepositDB txHash pk nCoin = do
  storeKey unspentOutputsLens (txHash, 0) (pk, nCoin)



processTransactionDB
  :: (Monad (q CoinStateDB), MonadFail (q CoinStateDB), ExecutorRW q)
  => Pet Tx
  -> q CoinStateDB ()
processTransactionDB transaction = case pet transaction of
  Deposit{} -> Control.Monad.Fail.fail ""
  Send pubK sig txSend@(pet -> TxSend{..}) -> do
    -- Signature must be valid
    check $ verifyPetrifiedSignature pubK txSend sig
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
    let txHash = hash transaction
    forM_ txInputs  $ dropKey  unspentOutputsLens
    forM_ ([0..] `zip` txOutputs) $ \(i,out) ->
      storeKey unspentOutputsLens (txHash,i) out
    where
      check True  = return ()
      check False = Control.Monad.Fail.fail ""

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

intToNetAddr :: Int -> NetAddr
intToNetAddr i = NetAddrV4 (fromIntegral i) 1122

netAddrToInt :: NetAddr -> Int
netAddrToInt (NetAddrV4 x 1122) = fromIntegral x
netAddrToInt na = error $ "invalid netaddr "++show na++" for conversion to int in Mock part of Thundermint"

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

genesisFromGenerator :: Pet (ValidatorSet Alg) -> GeneratorSpec -> Pet (Block Alg [Pet Tx])
genesisFromGenerator validatorSet GeneratorSpec{..} =
  makeGenesis "MONIES" (Time 0) dat validatorSet
  where
    dat = [ petrify $ Deposit pk genInitialDeposit | pk <- genInitialKeys ]

-- | Generate transaction. This implementation is really inefficient
--   since it will materialize all unspent outputs into memory and
--   shouldn't be used when number of wallets and coins is high
generateTransaction :: GeneratorSpec -> IO (EphemeralQ Alg [Pet Tx] CoinStateDB (Pet Tx))
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
        tx     = petrify TxSend
                   { txInputs  = map fst inputs
                   , txOutputs = [ (target, amount)
                                 , (pubK  , sum (map snd inputs) - amount)
                                 ]
                   }
    return $ petrify $ Send pubK (signPetrified privK tx) tx
  where
    nPK  = length genInitialKeys


-- | Generate transaction indefinitely
transactionGenerator
  :: (MonadIO m, MonadDB m Alg [Pet Tx], MonadFail m)
  => GeneratorSpec -> (Pet Tx -> m ()) -> m ()
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
  :: ( MonadIO m, MonadLogger m, MonadFork m, MonadTrace m, MonadMask m, MonadTMMonitoring m, MonadFail m)
  => Maybe Int64                       -- ^ Maximum height
  -> GeneratorSpec                     -- ^ Spec for generator of transactions
  -> Pet (ValidatorSet Ed25519_SHA512) -- ^ Set of validators
  -> BlockchainNet                    -- ^ Network
  -> Configuration cfg                 -- ^ Configuration for network
  -> NodeSpec                          -- ^ Node specifications
  -> m (Connection 'RO Alg [Pet Tx], m ())
interpretSpec maxHeight genSpec validatorSet net cfg NodeSpec{..} = do
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
        acts <- runNode cfg net
          NodeDescription
            { nodeValidationKey   = nspecPrivKey
            , nodeCommitCallback  = \case
                b | Just hM <- maxHeight
                  , headerHeight (pet (blockHeader b)) > Height hM -> throwM Abort
                  | otherwise                                      -> return ()
            , nodeReadyCreateBlock = \_ _ -> do n <- mempoolSize $ nodeMempool logic
                                                return $ n > 0
            }
          logic
        runConcurrently (generator : acts)
    )
  where
    genesisBlock :: Pet (Block Alg [Pet Tx])
    genesisBlock = genesisFromGenerator validatorSet genSpec

--
executeNodeSpec
  :: Maybe Int64                -- ^ Maximum height
  -> Int                        -- ^ Delay for generator
  -> NetSpec NodeSpec           -- ^ Specification for net
  -> IO [Connection 'RO Alg [Pet Tx]]
executeNodeSpec maxH delay NetSpec{..} = do
  net <- P2P.newMockNet
  let totalNodes   = length netNodeList
      netAddresses = Map.fromList $ [ intToNetAddr i | i <- [0..]] `zip` netNodeList
      connections  = case netTopology of
        Ring    -> connectRing
        All2All -> connectAll2All
      validatorSet = petrify
                   $ makeValidatorSetFromPriv [ pk | Just pk <- nspecPrivKey <$> netNodeList ]

  actions <- forM (Map.toList netAddresses) $ \(addr, nspec@NodeSpec{..}) -> do
    let genSpec = restrictGenerator (netAddrToInt addr) totalNodes
                $ defaultGenerator netInitialKeys netInitialDeposit delay
        bnet    = BlockchainNet
          { bchNetwork      = P2P.createMockNode net addr
          , bchLocalAddr    = addr
          , bchInitialPeers = connections netAddresses addr
          }
    let loggers = [ makeScribe s | s <- nspecLogFile ]
        run m   = withLogEnv "TM" "DEV" loggers $ \logenv -> runLoggerT logenv m
    run $ (fmap . fmap) run $ interpretSpec maxH genSpec validatorSet bnet netNetCfg nspec
  runConcurrently (snd <$> actions) `catch` (\Abort -> return ())
  return $ fst <$> actions
