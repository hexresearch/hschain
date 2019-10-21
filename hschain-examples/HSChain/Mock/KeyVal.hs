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
module HSChain.Mock.KeyVal (
    genesisBlock
  , interpretSpec
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

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Types.Blockchain
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

type    Alg    = Ed25519 :& SHA512
type    Tx     = (String,Int)
type    BState = Map String Int
newtype BData  = BData [(String,Int)]
  deriving stock    (Show,Eq,Generic)
  deriving anyclass (Serialise)

instance BlockData BData where
  type TX               BData = Tx
  type InterpreterState BData = BState
  blockTransactions (BData txs) = txs
  logBlockData      (BData txs) = HM.singleton "Ntx" $ JSON.toJSON $ length txs

genesisBlock :: ValidatorSet Alg -> Block Alg BData
genesisBlock valSet
  = makeGenesis (BData []) valSet

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
  store <- newSTMBchStorage mempty
  --
  let logic = AppLogic
        { appValidationFun    = \b (BlockchainState st valset) -> do
            return $ do st' <- foldM (flip process) st (let BData tx = blockData b in tx)
                        return $ BlockchainState st' valset
        , appBlockGenerator  = \b _ -> do
            let BlockchainState st valset = newBlockState b
                Just k = find (`Map.notMember` st) ["K_" ++ show (n :: Int) | n <- [1 ..]]
            i <- liftIO $ randomRIO (1,100)
            return (BData [(k, i)], BlockchainState (Map.insert k i st) valset)
        , appMempool         = nullMempool
        , appBchState        = store
        , appProposerChoice  = randomProposerSHA512
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
  resources <- traverse (\x -> do { r <- allocNode genesis x; return (x,r) })
             $ allocateMockNetAddrs net netTopology
             $ netNodeList
  -- Start nodes
  rnodes    <- lift $ forM resources $ \(x, (conn, logenv)) -> do
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
