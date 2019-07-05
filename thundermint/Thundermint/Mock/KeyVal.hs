{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}
-- |
module Thundermint.Mock.KeyVal (
    genesisBlock
  , transitions
  , executeSpec
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Maybe
import Data.List
import Data.Map.Strict             (Map)
import qualified Data.Map.Strict as Map
import System.Random   (randomRIO)

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
import Thundermint.Debug.Trace
import Thundermint.Types.Validators (ValidatorSet)
import qualified Thundermint.P2P.Network as P2P


----------------------------------------------------------------
--
----------------------------------------------------------------

type Alg    = Ed25519 :& SHA512
type Tx     = (String,Int)
type BState = Map String Int

genesisBlock :: ValidatorSet Alg -> Block Alg [Tx]
genesisBlock valSet
  = makeGenesis "KV" (Time 0) [] valSet

transitions :: BlockFold BState alg [Tx]
transitions = BlockFold
  { processTx           = const $ const process
  , processBlock        = \_ b s0 -> foldM (flip process) s0 (blockData b)
  , transactionsToBlock = \_ ->
      let selectTx _ []     = []
          selectTx c (t:tx) = case process t c of
                                Nothing -> selectTx c  tx
                                Just c' -> t : selectTx c' tx
      in selectTx
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
  :: ( MonadDB m Alg [Tx], MonadFork m, MonadMask m, MonadLogger m
     , MonadTrace m, MonadTMMonitoring m
     , Has x BlockchainNet
     , Has x NodeSpec
     , Has x (Configuration Example))
  => x
  -> AppCallbacks m Alg [Tx]
  -> m (RunningNode BState m Alg [Tx], [m ()])
interpretSpec p cb = do
  conn     <- askConnectionRO
  bchState <- newBChState transitions
  let logic = AppLogic
        { appValidationFun    = \valset b -> do
            let h = headerHeight $ blockHeader b
            st <- stateAtH bchState h
            return $ valset <$ processBlock transitions CheckSignature b st
        , appCommitQuery     = SimpleQuery $ \valset _ -> return valset
        , appBlockGenerator  = \_ h _ _ _ valset -> do
            st <- stateAtH bchState h
            let Just k = find (`Map.notMember` st) ["K_" ++ show (n :: Int) | n <- [1 ..]]
            i <- liftIO $ randomRIO (1,100)
            return ([(k, i)], valset)
        , appMempool         = nullMempoolAny
        }
  acts <- runNode (getT p :: Configuration Example) NodeDescription
    { nodeValidationKey = p ^.. nspecPrivKey
    , nodeCallbacks     = cb
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


executeSpec
  :: (MonadIO m, MonadMask m, MonadFork m, MonadTrace m, MonadTMMonitoring m)
  => NetSpec NodeSpec
  -> ContT r m [RunningNode BState m Alg [Tx]]
executeSpec NetSpec{..} = do
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
  -- Actually run nodes
  lift   $ catchAbort $ runConcurrently $ snd =<< rnodes
  return $ fst <$> rnodes
  where
    valSet  = makeValidatorSetFromPriv $ catMaybes [ x ^.. nspecPrivKey | x <- netNodeList ]
    genesis = genesisBlock valSet
