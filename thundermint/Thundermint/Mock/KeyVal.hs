{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
-- |
module Thundermint.Mock.KeyVal (
    genesisBlock
  , transitions
  , executeSpec
  , BData(..)
  , Tx
  , BState
  ) where

import Codec.Serialise (Serialise)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Maybe
import Data.List
import Data.Map.Strict                 (Map)
import qualified Data.Aeson          as JSON
import qualified Data.Map.Strict     as Map
import qualified Data.HashMap.Strict as HM
import System.Random   (randomRIO)
import GHC.Generics    (Generic)

import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Blockchain.Interpretation
import Thundermint.Types.Blockchain
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Ed25519
import Thundermint.Crypto.SHA
import Thundermint.Logger
import Thundermint.Mock.KeyList
import Thundermint.Mock.Types
import Thundermint.Monitoring
import Thundermint.Run
import Thundermint.Store
import Thundermint.Store.STM
import Thundermint.Debug.Trace
import Thundermint.Types.Validators (ValidatorSet)
import qualified Thundermint.P2P.Network as P2P


----------------------------------------------------------------
--
----------------------------------------------------------------

type    Alg    = Ed25519 :& SHA512
type    Tx     = (String,Int)
type    BState = Map String Int
newtype BData  = BData [(String,Int)]
  deriving stock    (Show,Eq,Generic)
  deriving anyclass (Serialise)

instance BlockData BData where
  type TX              BData = Tx
  type BlockchainState BData = BState
  blockTransactions (BData txs) = txs
  logBlockData      (BData txs) = HM.singleton "Ntx" $ JSON.toJSON $ length txs

genesisBlock :: ValidatorSet Alg -> Block Alg BData
genesisBlock valSet
  = makeGenesis "KV" (Time 0) (BData []) valSet

transitions :: BlockFold alg BData
transitions = BlockFold
  { processTx           = const $ const process
  , processBlock        = \_ b s0 -> foldM (flip process) s0 (blockTransactions $  blockData b)
  , transactionsToBlock = \_ ->
      let selectTx _ []     = []
          selectTx c (t:tx) = case process t c of
                                Nothing -> selectTx c  tx
                                Just c' -> t : selectTx c' tx
      in (fmap . fmap) BData selectTx
  , initialState        = Map.empty
  }
  where


process :: Tx -> BState -> Maybe BState
process (k,v) m
  | k `Map.member` m = Nothing
  | otherwise        = Just $ Map.insert k v m



-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

interpretSpec
  :: ( MonadDB m Alg BData, MonadFork m, MonadMask m, MonadLogger m
     , MonadTrace m, MonadTMMonitoring m
     , Has x BlockchainNet
     , Has x NodeSpec
     , Has x (Configuration Example))
  => x
  -> AppCallbacks m Alg BData
  -> m (RunningNode m Alg BData, [m ()])
interpretSpec p cb = do
  conn  <- askConnectionRO
  store <- newSTMBchStorage $ initialState transitions
  rewindBlockchainState transitions store
  let logic = AppLogic
        { appValidationFun    = \valset b st -> do
            return $ do st' <- processBlock transitions CheckSignature b st
                        return (valset,st')
        , appCommitQuery     = SimpleQuery $ \valset _ -> return valset
        , appBlockGenerator  = \valset _ st _ -> do
            let Just k = find (`Map.notMember` st) ["K_" ++ show (n :: Int) | n <- [1 ..]]
            i <- liftIO $ randomRIO (1,100)
            return (BData [(k, i)], valset)
        , appMempool         = nullMempoolAny
        , appBchState        = store
        }
  acts <- runNode (getT p :: Configuration Example) NodeDescription
    { nodeValidationKey = p ^.. nspecPrivKey
    , nodeCallbacks     = cb
    , nodeLogic         = logic
    , nodeNetwork       = getT p
    }
  return
    ( RunningNode { rnodeState   = store
                  , rnodeConn    = conn
                  , rnodeMempool = appMempool logic
                  }
    , acts
    )


executeSpec
  :: (MonadIO m, MonadMask m, MonadFork m, MonadTrace m, MonadTMMonitoring m)
  => NetSpec NodeSpec
  -> ContT r m [RunningNode m Alg BData]
executeSpec NetSpec{..} = do
  -- Create mock network and allocate DB handles for nodes
  net       <- liftIO P2P.newMockNet
  resources <- traverse (allocNode genesis)
             $ allocateMockNetAddrs net netTopology
             $ netNodeList
  -- Start nodes
  rnodes    <- lift $ forM resources $ \(x, conn, logenv) -> do
    let run :: DBT 'RW Alg BData (LoggerT m) x -> m x
        run = runLoggerT logenv . runDBT conn
    (rn, acts) <- run $ interpretSpec (netNetCfg :*: x)
      (maybe mempty callbackAbortAtH netMaxH)
    return ( hoistRunningNode run rn
           , run <$> acts
           )
  -- Actually run nodes
  lift   $ catchAbort $ runConcurrently $ snd =<< rnodes
  return $ fst <$> rnodes
  where
    valSet  = makeValidatorSetFromPriv $ catMaybes [ x ^.. nspecPrivKey | x <- netNodeList ]
    genesis = genesisBlock valSet
