{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
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
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Concurrent   (threadDelay)
import Control.DeepSeq
import Codec.Serialise      (Serialise,serialise)
import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.Maybe
import Data.Map             (Map)
import qualified Data.Vector      as V
import qualified Data.Map.Strict  as Map
import System.Random   (randomRIO)
import GHC.Generics    (Generic)

import Thundermint.P2P
import Thundermint.Types.Blockchain
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
import Thundermint.Store.Internal.Query (connectionRO)
import Thundermint.Mock.KeyList (privateKeyList)
import Thundermint.Mock.Types
import Thundermint.Monitoring
import Thundermint.Types.Validators (ValidatorSet)
import qualified Thundermint.P2P.Network as P2P


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

{-
-- | Specification of generator of transactions
data TxGenerator = TxGenerator
  { genPrivateKeys :: V.Vector (PrivKey Alg)
    -- ^ Private keys for which we can generate transactions
  , genDestinaions :: V.Vector (PublicKey Alg)
    -- ^ List of all addresses to which we can send money
  , genDelay       :: Int
    -- ^ Delay between invokations of generator
  }

transactionGenerator
  :: MonadIO m
  => TxGenerator
  -> m CoinState
  -> (Tx -> m ())
  -> m a
transactionGenerator gen coinState push = forever $ do
  push =<< generateTransaction gen =<< coinState
  liftIO $ threadDelay $ genDelay gen * 1000

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
-}


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
netAddrToInt na = error $ "Invalid NetAddr " ++ show na ++ " for conversion to Int in Mock part of Thundermint"

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
genesisFromGenerator validatorSet GeneratorSpec{..} =
  makeGenesis "MONIES" (Time 0) dat validatorSet
  where
    dat = [ Deposit pk genInitialDeposit | pk <- genInitialKeys ]

-- | Generate transaction. This implementation is really inefficient
--   since it will materialize all unspent outputs into memory and
--   shouldn't be used when number of wallets and coins is high
generateTransaction :: GeneratorSpec -> CoinState -> IO Tx
generateTransaction GeneratorSpec{..} (CoinState utxo)= do
  -- Pick private key
  privK <- do i <- randomRIO (0, length genPrivateKeys - 1)
              return (genPrivateKeys !! i)
  let pubK = publicKey privK
  -- Pick public key to send data to
  target <- do i <- randomRIO (0, nPK - 1)
               return (genInitialKeys !! i)
  amount <- randomRIO (0,20)
  -- Create transaction
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
  :: (MonadIO m, MonadDB m Alg [Tx], MonadFail m)
  => GeneratorSpec
  -> BChState m CoinState
  -> (Tx -> m ())
  -> m ()
transactionGenerator gen bchState push = forever $ do
  st <- currentState bchState
  tx <- liftIO $ generateTransaction gen st
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
  => Maybe Height                     -- ^ Maximum height
  -> GeneratorSpec                    -- ^ Spec for generator of transactions
  -> Block Alg [Tx]                   -- ^ Genesis
  -> BlockchainNet                    -- ^ Network
  -> Configuration cfg                -- ^ Configuration for network
  -> NodeSpec                         -- ^ Node specifications
  -> m (Connection 'RO Alg [Tx], m ())
interpretSpec maxHeight genSpec genesisBlock net cfg NodeSpec{..} = do
  -- Allocate storage for node
  conn <- openConnection (maybe ":memory:" id nspecDbName)
  initDatabase conn genesisBlock
  return
    ( connectionRO conn
    , runDBT conn $ do
        (bchState,logic) <- logicFromFold transitions
        -- Transactions generator
        cursor <- getMempoolCursor $ appMempool logic
        let generator = transactionGenerator genSpec bchState (void . pushTransaction cursor)
        acts <- runNode cfg NodeDescription
          { nodeValidationKey = nspecPrivKey
          , nodeCallbacks     = maybe mempty callbackAbortAtH maxHeight
                             <> nonemptyMempoolCallback (appMempool logic)
          , nodeLogic         = logic
          , nodeNetwork       = net
          }
        runConcurrently ( generator : acts)
    )

--
executeNodeSpec
  :: NetSpec NodeSpec :*: CoinSpecification
  -> IO [Connection 'RO Alg [Tx]]
executeNodeSpec (NetSpec{..} :*: CoinSpecification{..}) = do
  net <- P2P.newMockNet
  let totalNodes   = length netNodeList
      validatorSet = makeValidatorSetFromPriv [ pk | Just pk <- nspecPrivKey <$> netNodeList ]
      -- FIXME: wrong
      defGenSpec   = defaultGenerator coinWallets coinAridrop (fromMaybe 0 coinGeneratorDelay)
      genesisBlock = genesisFromGenerator validatorSet defGenSpec
  --
  actions <- forM (allocateMockNetAddrs net netTopology netNodeList) $ \(nspec@NodeSpec{..}, addr, bnet) -> do
    let genSpec = restrictGenerator (netAddrToInt addr) totalNodes
                $ defGenSpec
    let loggers = [ makeScribe s | s <- nspecLogFile ]
        run m   = withLogEnv "TM" "DEV" loggers $ \logenv -> runLoggerT logenv m
    run $ (fmap . fmap) run $ interpretSpec netMaxH genSpec genesisBlock bnet netNetCfg nspec
  runConcurrently (snd <$> actions) `catch` (\Abort -> return ())
  return $ fst <$> actions


allocateMockNetAddrs :: P2P.MockNet -> Topology -> [a] -> [(a, NetAddr, BlockchainNet)]
allocateMockNetAddrs net topo nodes =
  [ ( n
    , addr
    , BlockchainNet { bchNetwork      = P2P.createMockNode net addr
                    , bchInitialPeers = connections addresses addr
                    })
  | (addr, n) <- Map.toList addresses
  ]
  where
    addresses   = Map.fromList $ [ intToNetAddr i | i <- [0..]] `zip` nodes
    connections = case topo of
        Ring    -> connectRing
        All2All -> connectAll2All


----------------------------------------------------------------
-- Specialized ode runners
----------------------------------------------------------------

withNodeSpec
  :: ( MonadIO m, MonadMask m
     , Crypto alg, Serialise a, Eq a, Show a
     , Has x NodeSpec
     )
  => Block alg a
  -> x
  -> (x -> DBT 'RW alg a (LoggerT m) r)
  -> m r
withNodeSpec genesis x action = evalContT $ do
  conn   <- ContT $ withDatabase (fromMaybe "" $ x ^.. nspecDbName) genesis
  logenv <- ContT $ withLogEnv "TM" "DEV" [ makeScribe s | s <- x ^.. nspecLogFile ]
  lift $ runLoggerT logenv
       $ runDBT conn
       $ action x


withNodeSpecMany
  :: ( MonadIO m, MonadMask m, MonadFork m
     , Crypto alg, Serialise a, Eq a, Show a, Traversable t
     , Has x NodeSpec
     )
  => Block alg a
  -> t x
  -> (x -> DBT 'RW alg a (LoggerT m) r)
  -> (t (m r) -> m q)
  -> m q
withNodeSpecMany genesis xs action cont = evalContT $ do
  pars <- traverse acquire xs
  lift $ cont $ fini <$> pars
  where
    fini (x,conn,logenv) = runLoggerT logenv
                         $ runDBT conn
                         $ action x
    acquire x = do
      conn   <- ContT $ withDatabase (fromMaybe "" $ x ^.. nspecDbName) genesis
      logenv <- ContT $ withLogEnv "TM" "DEV" [ makeScribe s | s <- x ^.. nspecLogFile ]
      return (x,conn,logenv)
