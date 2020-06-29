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
module HSChain.Mock.KeyVal {-(
    mkGenesisBlock
  , interpretSpec
  , executeSpec
  , keyValLogic
  , BData(..)
  , Tx
  , BState
  )-} where

import Control.Concurrent.STM
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
import HSChain.Control.Class
import HSChain.Control.Util
import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.Internal.Types.Consensus
import HSChain.Logger
import HSChain.Mempool
import HSChain.Mock.KeyList
import HSChain.Mock.Types
import HSChain.Monitoring
import HSChain.Run
import HSChain.Mock
import HSChain.Store
import HSChain.Types.Validators
import qualified HSChain.Network.Mock as P2P


----------------------------------------------------------------
--
----------------------------------------------------------------

type    Tx     = (String,Int)
type    BState = Map String Int
newtype BData  = BData { unBData :: [(String,Int)] }
  deriving stock    (Show,Eq,Generic)
  deriving anyclass (Serialise)
  deriving newtype  (JSON.ToJSON, JSON.FromJSON)
instance CryptoHashable BData where
  hashStep = genericHashStep "hschain-examples"

data KeyValError = KeyValError String
  deriving stock    (Show) 
  deriving anyclass (Exception)

instance BlockData BData where
  type TX       BData = Tx
  type BChError BData = KeyValError
  type Alg      BData = Ed25519 :& SHA512
  proposerSelection        = ProposerSelection randomProposerSHA512
  logBlockData (BData txs) = HM.singleton "Ntx" $ JSON.toJSON $ length txs


mkGenesisBlock :: ValidatorSet (Alg BData) -> Genesis BData
mkGenesisBlock valSet = Genesis
  { genesisBlock  = makeGenesis (BData []) valSet valSet
  , genesisValSet = valSet
  }


-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- | Create view on blockchain state which is kept completely in
--   memory
inMemoryStateView :: MonadIO m => m (StateView m BData, IO BState)
inMemoryStateView = do
  stRef <- liftIO $ newTVarIO (Nothing, mempty)
  let makeCommit valSet st' h = UncommitedState
        { commitState   = atomicallyIO $ writeTVar stRef (Just h, st')
        , newValidators = valSet
        }
  return
    ( StateView
      { validatePropBlock = \b valSet -> do
          (_,st) <- liftIO $ readTVarIO stRef
          return $ do
            st' <- foldM (flip process) st
                 $ unBData $ merkleValue $ blockData b
            return $ makeCommit valSet st' (blockHeight b)
      , stateHeight       = liftIO $ fst <$> readTVarIO stRef
      , generateCandidate = \NewBlock{..} -> do
          (_,st) <- liftIO $ readTVarIO stRef          
          i <- liftIO $ randomRIO (1,100)
          let Just k = find (`Map.notMember` st)
                       ["K_" ++ show (n :: Int) | n <- [1 ..]]
          let tx        = (k,i)
              Right st' = process tx st
          return ( BData [tx]
                 , makeCommit newBlockValSet st' newBlockHeight
                 )
      , stateMempool      = nullMempool
      }
    , snd <$> readTVarIO stRef
    )

process :: Tx -> BState -> Either KeyValError BState
process (k,v) m
  | k `Map.member` m = Left  $ KeyValError k
  | otherwise        = Right $! Map.insert k v m



interpretSpec
  :: ( MonadDB BData m, MonadFork m, MonadMask m, MonadLogger m
     , MonadTMMonitoring m )
  => Genesis BData
  -> NodeSpec BData
  -> BlockchainNet
  -> Configuration Example
  -> AppCallbacks m BData
  -> m (StateView m BData, [m ()])
interpretSpec genesis nspec bnet cfg cb = do
  (state,_) <- inMemoryStateView
  acts <- runNode cfg NodeDescription
    { nodeValidationKey = nspecPrivKey nspec
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb
    , nodeNetwork       = bnet
    , nodeStateView     = state
    }
  return
    ( state
    , acts
    )



-- executeSpec
--   :: (MonadIO m, MonadMask m, MonadFork m, MonadTMMonitoring m)
--   => NetSpec (NodeSpec BData)
--   -> ContT r m [RunningNode m BData]
-- executeSpec NetSpec{..} = do
--   -- Create mock network and allocate DB handles for nodes
--   net       <- liftIO P2P.newMockNet
--   resources <- allocNetwork net netTopology netNodeList
--   -- Start nodes
--   rnodes    <- lift $ forM resources $ \(nspec, bnet, conn, logenv) -> do
--     let run :: DBT 'RW BData (LoggerT m) x -> m x
--         run = runLoggerT logenv . runDBT conn
--     (rn, acts) <- run $ interpretSpec
--       genesis
--       nspec
--       bnet
--       netNetCfg
--       (maybe mempty callbackAbortAtH netMaxH)
--     return ( hoistRunningNode run rn
--            , run <$> acts
--            )
--   -- Actually run nodes
--   lift   $ catchAbort $ runConcurrently $ snd =<< rnodes
--   return $ fst <$> rnodes
--   where
--     valSet  = makeValidatorSetFromPriv $ catMaybes [ nspecPrivKey x | x <- netNodeList ]
--     genesis = mkGenesisBlock valSet
