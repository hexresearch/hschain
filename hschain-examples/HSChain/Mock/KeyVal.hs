{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-- |
module HSChain.Mock.KeyVal (
    mkGenesisBlock
  , interpretSpec
  , executeSpec
  , keyValLogic
  , BData(..)
  , Tx
  , BState
  ) where

import Codec.Serialise (Serialise)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad.Morph
import Data.Maybe
import Data.List
import Data.Map.Strict                 (Map)
import qualified Data.Aeson          as JSON
import qualified Data.Map.Strict     as Map
import qualified Data.HashMap.Strict as HM
import System.Random   (randomRIO)
import GHC.Generics    (Generic)

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
import HSChain.Control
import HSChain.Crypto
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.Logger
import HSChain.Mock.KeyList
import HSChain.Mock.Types
import HSChain.Monitoring
import HSChain.Run
import HSChain.Mock
import HSChain.Store
import HSChain.Store.STM
import HSChain.Debug.Trace
import HSChain.Types.Validators
import qualified HSChain.P2P.Network as P2P


----------------------------------------------------------------
--
----------------------------------------------------------------

type    Tx     = (String,Int)
type    BState = Map String Int
newtype BData  = BData [(String,Int)]
  deriving stock    (Show,Eq,Generic)
  deriving anyclass (Serialise)
  deriving newtype  (CryptoHashable, JSON.ToJSON, JSON.FromJSON)


data KeyValError = KeyValError String
  deriving stock    (Show) 
  deriving anyclass (Exception)

instance BlockData BData where
  type TX              BData = Tx
  type BlockchainState BData = BState
  type BChError        BData = KeyValError
  type Alg             BData = Ed25519 :& SHA512
  type BChMonad        BData = ExceptT KeyValError IO
  blockTransactions (BData txs) = txs
  logBlockData      (BData txs) = HM.singleton "Ntx" $ JSON.toJSON $ length txs
  proposerSelection             = ProposerSelection randomProposerSHA512
  bchLogic                      = keyValLogic

mkGenesisBlock :: ValidatorSet (Alg BData) -> Genesis BData
mkGenesisBlock valSet = BChEval
  { bchValue        = makeGenesis (BData []) (hashed mempty) valSet valSet
  , validatorSet    = valSet
  , blockchainState = mempty
  }


-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

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
  conn  <- askConnectionRO
  store <- newSTMBchStorage mempty
  --
  let astore = AppStore
        { appMempool  = nullMempool
        , appBchState = store
        }
  acts <- runNode (getT p :: Configuration Example) NodeDescription
    { nodeValidationKey = p ^.. nspecPrivKey
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb
    , nodeRunner        = hoist liftIO
    , nodeStore         = astore
    , nodeNetwork       = getT p
    }
  return
    ( RunningNode { rnodeState   = store
                  , rnodeConn    = conn
                  , rnodeMempool = appMempool astore
                  }
    , acts
    )

keyValLogic :: MonadIO m => BChLogic (ExceptT KeyValError m) BData
keyValLogic = BChLogic
  -- We don't use mempool here so we can just use trivial handler for
  -- transactions
  { processTx     = \_ -> throwE $ KeyValError ""
  --
  , processBlock  = \BChEval{..} -> ExceptT $ return $ do
      st <- foldM (flip process) blockchainState
          $ blockTransactions $ merkleValue $ blockData bchValue
      return BChEval { bchValue        = ()
                     , blockchainState = st
                     , ..
                     }
  --
  , generateBlock = \NewBlock{..} _ -> do
      let Just k = find (`Map.notMember` newBlockState) ["K_" ++ show (n :: Int) | n <- [1 ..]]
      i <- liftIO $ randomRIO (1,100)
      return BChEval { bchValue        = BData [(k, i)]
                     , validatorSet    = newBlockValSet
                     , blockchainState = Map.insert k i newBlockState
                     }
  }
  where
    process :: Tx -> BState -> Either KeyValError BState
    process (k,v) m
      | k `Map.member` m = Left  $ KeyValError k
      | otherwise        = Right $! Map.insert k v m



executeSpec
  :: (MonadIO m, MonadMask m, MonadFork m, MonadTrace m, MonadTMMonitoring m)
  => NetSpec NodeSpec
  -> ContT r m [RunningNode m BData]
executeSpec NetSpec{..} = do
  -- Create mock network and allocate DB handles for nodes
  net       <- liftIO P2P.newMockNet
  resources <- traverse (\x -> do { r <- allocNode x; return (x,r) })
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
  -- Actually run nodes
  lift   $ catchAbort $ runConcurrently $ snd =<< rnodes
  return $ fst <$> rnodes
  where
    valSet  = makeValidatorSetFromPriv $ catMaybes [ x ^.. nspecPrivKey | x <- netNodeList ]
    genesis = mkGenesisBlock valSet
