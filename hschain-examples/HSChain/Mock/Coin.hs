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
  -- , coinLogic
    -- * Transaction generator
  , mintMockCoin
  , transactionGenerator
  , TxGenerator(..)
    -- * Interpretation
  , interpretSpec
  , RunningNode(..)
  , executeNodeSpec
  ) where

import Codec.Serialise
import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Foldable
import Data.Functor.Identity
import Data.Either
import Data.IORef
import Data.Maybe
import Data.Map             (Map,(!))
import qualified Data.Vector         as V
import qualified Data.Map.Strict     as Map
import qualified Data.IntMap.Strict  as IMap
import qualified Data.Set            as Set
import System.Random   (randomRIO)
import GHC.Generics    (Generic)

import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
import HSChain.Types.Validators
import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control.Class
import HSChain.Control.Util
import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Logger
import HSChain.Mempool
import HSChain.Run
import HSChain.Internal.Types.Consensus
import HSChain.Mock
import HSChain.Mock.Coin.Types
import HSChain.Store
import HSChain.Mock.KeyList         (makePrivKeyStream)
import HSChain.Mock.Types
import HSChain.Monitoring
import qualified HSChain.Network.Mock as P2P



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
inMemoryStateView :: MonadIO m => m (StateView m BData, [m ()], IO CoinState)
inMemoryStateView = do
  stRef                <- liftIO $ newIORef (Nothing, CoinState mempty mempty)
  dict@MempoolDict{..} <- newMempoolDict (isRight . validateTxContextFree)
  let makeCommit valSet txList st' h = UncommitedState
        { commitState = do
            -- First we remote TX that were commited
            atomicallyIO $ modifyTVar' varMempool $
              mempoolRemoveTX (map hashed txList)
            -- Then we ask mempool to start filtering rest of TXs
            atomicallyIO $ writeTChan chPushTx $ CmdStartFiltering $
              \tx -> return $ isRight $ processSend tx st'
            liftIO $ writeIORef stRef (Just h, st')
        , newValidators = valSet
        }
  return
    ( StateView
      { validatePropBlock = \b valSet -> do
          (_,st) <- liftIO $ readIORef stRef
          return $ do
            -- When we validate proposed block we want to do complete validation
            let step s tx = do
                  validateTxContextFree tx
                  processTxFull (blockHeight b) tx s
            let txList = unBData $ merkleValue $ blockData b
            st' <- foldM step st txList
            return $ makeCommit valSet txList st' (blockHeight b)
        --
      , stateHeight = liftIO $ fst <$> readIORef stRef
        --
      , generateCandidate = \NewBlock{..} -> do
          (_,st) <- liftIO $ readIORef  stRef
          mem    <- liftIO $ readTVarIO varMempool
          let selectTx c []     = (c,[])
              selectTx c (t:tx) = case processSend t c of
                                    Left  _  -> selectTx c  tx
                                    Right c' -> let (c'', b  ) = selectTx c' tx
                                                in  (c'', t:b)                                     
          let (st', dat) = selectTx st
                         $ map merkleValue
                         $ toList
                         $ mempFIFO mem
          return
            ( BData dat
            , makeCommit newBlockValSet dat st' newBlockHeight
            )
        -- Here we construct handle to memepool
      , mempoolHandle = makeMempoolHandle dict
      }
    , [makeMempoolThread dict]
    , snd <$> readIORef stRef
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




-- -- | Complete description of blockchain logic. Since we keep all state
-- --   in memory we simply use @Maybe@ to track failures
-- coinLogic :: BChLogic (Either CoinError) BData
-- coinLogic = BChLogic
--   { processTx     = \BChEval{..} -> void $ processTransaction bchValue (merkleValue blockchainState)
--   --
--   , processBlock  = \BChEval{..} -> do
--       let h    = blockHeight bchValue
--           step = flip $ process h
--       st <- foldM step (merkleValue blockchainState)
--           $ let BData txs = merkleValue $ blockData bchValue
--             in txs
--       return BChEval { bchValue        = ()
--                      , blockchainState = merkled st
--                      , ..
--                      }
--   --
--   , generateBlock = \NewBlock{..} txs -> do
--       let selectTx c []     = (c,[])
--           selectTx c (t:tx) = case processTransaction t c of
--                                 Left  _  -> selectTx c  tx
--                                 Right c' -> let (c'', b  ) = selectTx c' tx
--                                             in  (c'', t:b)
--       let (st', dat) = selectTx (merkleValue newBlockState) txs
--       return BChEval { bchValue        = BData dat
--                      , validatorSet    = merkled newBlockValSet
--                      , blockchainState = merkled st'
--                      }
--   }
--   where
--     process (Height 0) t s = case processDeposit t s of
--       Right x -> Right x
--       Left  _ -> processTransaction t s
--     process _          t s = processTransaction t s




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
  -> MempoolHandle (Alg BData) Tx
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
  , Genesis
    { genesisBlock  = genesis0
    , genesisValSet = valSet
    -- , blockchainState = merkled state0
    }
  )
  where
    privK        = take coinWallets $ makePrivKeyStream coinWalletsSeed
    pubK         = publicKey <$> privK
    Right valSet = makeValidatorSet nodes
    txs          = [ Deposit pk coinAirdrop | pk <- pubK ]
    -- Generate genesis with correct hash
    -- state0       = CoinState mempty mempty
    genesis0     = makeGenesis (BData txs) valSet valSet


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
  -> m (RunningNode m BData, [m ()], IO CoinState)
interpretSpec genesis cfg net spec cb = do
  conn                  <- askConnectionRO
  (state,memThr,readST) <- inMemoryStateView
  acts <- runNode cfg NodeDescription
    { nodeValidationKey = nspecPrivKey spec
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb
    , nodeNetwork       = net
    , nodeStateView     = state
    }
  return
    ( RunningNode { rnodeState   = state
                  , rnodeConn    = conn
                  , rnodeMempool = mempoolHandle state
                  }
    , memThr <> acts
    , readST
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
    (rn, acts, readST) <- run $ interpretSpec
      genesis
      netNetCfg
      bnet
      spec
      (maybe mempty callbackAbortAtH netMaxH)
    return ( hoistRunningNode run rn
           , run <$> acts
           , readST
           )
  -- Allocate transactions generators
  txGens <- lift $ case mtxGen of
    Nothing  -> return []
    Just txG -> forM rnodes $ \(RunningNode{..}, _, readST) -> do
      cursor <- getMempoolCursor rnodeMempool
      return $ transactionGenerator txG
        rnodeMempool
        (liftIO readST)
        (pushTxAsync cursor)
  -- Actually run nodes
  lift   $ catchAbort $ runConcurrently $ ((\(_,x,_) -> x) =<< rnodes) ++ txGens
  return $ (\(x,_,_) -> x) <$> rnodes
  where
    (mtxGen, genesis) = mintMockCoin
      [ Validator (publicKey k) 1
      | Just (PrivValidator k) <- nspecPrivKey <$> netNodeList
      ] coin

