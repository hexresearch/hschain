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
import HSChain.Control.Class
import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
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
import HSChain.Types.Validators
import qualified HSChain.Network.Mock as P2P


----------------------------------------------------------------
--
----------------------------------------------------------------

type    Tx     = (String,Int)
type    BState = Map String Int
newtype BData  = BData [(String,Int)]
  deriving stock    (Show,Eq,Generic)
  deriving anyclass (Serialise)
  deriving newtype  (JSON.ToJSON, JSON.FromJSON)
instance CryptoHashable BData where
  hashStep = genericHashStep "hschain-examples"

data KeyValError = KeyValError String
  deriving stock    (Show) 
  deriving anyclass (Exception)

instance BlockData BData where
  type TX              BData = Tx
  type BlockchainState BData = BState
  type BChError        BData = KeyValError
  type Alg             BData = Ed25519 :& SHA512
  type BChMonad        BData = ExceptT KeyValError IO
  proposerSelection        = ProposerSelection randomProposerSHA512
  bchLogic                 = keyValLogic
  logBlockData (BData txs) = HM.singleton "Ntx" $ JSON.toJSON $ length txs


mkGenesisBlock :: ValidatorSet (Alg BData) -> Genesis BData
mkGenesisBlock valSet = BChEval
  { bchValue        = makeGenesis (BData []) (hashed mempty) valSet valSet
  , validatorSet    = merkled valSet
  , blockchainState = merkled mempty
  }


-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

interpretSpec
  :: ( MonadDB m BData, MonadFork m, MonadMask m, MonadLogger m
     , MonadTMMonitoring m
     , Has x BlockchainNet
     , Has x (NodeSpec BData)
     , Has x (Configuration Example))
  => Genesis BData
  -> x
  -> AppCallbacks m BData
  -> m (RunningNode m BData, [m ()])
interpretSpec genesis p cb = do
  conn  <- askConnectionRO
  store <- maybe return snapshotState (nspecPersistIval (getT p :: NodeSpec BData))
       =<< newSTMBchStorage (merkled mempty)
  --
  let astore = AppStore
        { appMempool  = nullMempool
        , appBchState = store
        }
  acts <- runNode (getT p :: Configuration Example) NodeDescription
    { nodeValidationKey = nspecPrivKey (getT p :: NodeSpec BData)
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
      st <- foldM (flip process) (merkleValue blockchainState)
          $ let BData txs = merkleValue $ blockData bchValue
            in txs
      return BChEval { bchValue        = ()
                     , blockchainState = merkled st
                     , ..
                     }
  --
  , generateBlock = \NewBlock{..} _ -> do
      let Just k = find (`Map.notMember` merkleValue newBlockState)
                   ["K_" ++ show (n :: Int) | n <- [1 ..]]
      i <- liftIO $ randomRIO (1,100)
      return BChEval { bchValue        = BData [(k, i)]
                     , validatorSet    = merkled newBlockValSet
                     , blockchainState = merkled $ Map.insert k i $ merkleValue newBlockState
                     }
  }
  where
    process :: Tx -> BState -> Either KeyValError BState
    process (k,v) m
      | k `Map.member` m = Left  $ KeyValError k
      | otherwise        = Right $! Map.insert k v m



executeSpec
  :: (MonadIO m, MonadMask m, MonadFork m, MonadTMMonitoring m)
  => NetSpec (NodeSpec BData)
  -> ContT r m [RunningNode m BData]
executeSpec NetSpec{..} = do
  -- Create mock network and allocate DB handles for nodes
  net       <- liftIO P2P.newMockNet
  resources <- traverse (\x -> do { r <- allocNode (getT x); return (x,r) })
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
