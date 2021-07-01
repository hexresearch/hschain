{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-- |
module HSChain.Mock.KeyVal (
    mkGenesisBlock
  , BData(..)
  , Tx
  , BState
    -- * Running KeyVal
  , KeyValDictM(..)
  , KeyValT(..)
  , runKeyValT
  , interpretSpec
  , executeSpec
  ) where

import Codec.Serialise (Serialise)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Data.Maybe
import Data.List
import Data.Map.Strict                 (Map)
import qualified Data.Aeson          as JSON
import qualified Data.Map.Strict     as Map
import qualified Data.HashMap.Strict as HM
import Katip           (Namespace,LogEnv)
import System.Random   (randomRIO)
import GHC.Generics    (Generic)

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Types.Blockchain
import HSChain.Types.Merkle.Types
import HSChain.Control.Class
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
  deriving stock    (Show,Generic)
  deriving anyclass (Exception,JSON.FromJSON,JSON.ToJSON)

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

data KeyValState = KeyValState
  { _kvHeight :: Maybe Height
  , _kvValSet :: ValidatorSet (Alg BData)
  , _kvState  :: Map.Map String Int
  }
$(makeLenses ''KeyValState)


instance StateView KeyValState where
  type BlockType       KeyValState = BData
  type ViewConstraints KeyValState = MonadIO
  stateHeight       = _kvHeight
  newValidators     = _kvValSet
  commitState       = pure
  validatePropBlock st b valSet = pure $ do
    st' <- foldM (flip process) (st^.kvState)
         $ unBData $ merkleValue $ blockData b
    return $ KeyValState
      { _kvHeight = Just $ blockHeight b
      , _kvValSet = valSet
      , _kvState  = st'
      }
  generateCandidate st NewBlock{..} = do
    i <- liftIO $ randomRIO (1,100)
    let Just k = find (`Map.notMember` (st^.kvState))
                 ["K_" ++ show (n :: Int) | n <- [1 ..]]
    let tx        = (k,i)
        Right st' = process tx $ st ^. kvState
    return ( BData [tx]
           , KeyValState
               { _kvHeight = Just newBlockHeight
               , _kvValSet = newBlockValSet
               , _kvState  = st'
               }
           )
  
  {-
-- | Create view on blockchain state which is kept completely in
--   memory
inMemoryStateView :: MonadIO m => ValidatorSet (Alg BData) -> StateView m BData
inMemoryStateView = make Nothing mempty
  where
    make mh st vals = r where
      r = StateView
        { stateHeight   = mh
        , newValidators = vals
        , commitState   = return r
        , validatePropBlock = \b valSet -> return $ do
        , generateCandidate = \NewBlock{..} -> do
        , stateMempool = nullMempool hashed
        }
-}
process :: Tx -> BState -> Either KeyValError BState
process (k,v) m
  | k `Map.member` m = Left  $ KeyValError k
  | otherwise        = Right $! Map.insert k v m



interpretSpec
  :: ( MonadDB m, MonadCached BData m, MonadFork m, MonadMask m, MonadLogger m
     , MonadTMMonitoring m )
  => Genesis BData
  -> NodeSpec BData
  -> BlockchainNet
  -> Configuration Example
  -> AppCallbacks m BData
  -> m (KeyValState, [m ()])
interpretSpec genesis nspec bnet cfg cb = do
  let state = KeyValState { _kvHeight = Nothing
                          , _kvValSet = genesisValSet genesis
                          , _kvState  = mempty
                          }
  acts  <- runNode cfg NodeDescription
    { nodeValidationKey = nspecPrivKey nspec
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb
    , nodeNetwork       = bnet
    , nodeStateView     = state
    }
    (nullMempool hashed)
  return
    ( state
    , acts
    )


-- | Parameters for 'CoinT' monad transformer
data KeyValDictM = KeyValDictM
  { dictNamespace :: !Namespace
  , dictLogEnv    :: !LogEnv
  , dictConn      :: !(Connection 'RW)
  , dictCached    :: !(Cached BData)
  }
  deriving stock (Generic)

-- | Application monad for key-value blockchain
newtype KeyValT m a = KeyValT { unKeyValT :: ReaderT KeyValDictM m a }
  deriving newtype (Functor,Applicative,Monad,MonadIO)
  deriving newtype (MonadThrow,MonadCatch,MonadMask,MonadFork)
  deriving newtype (MonadReader KeyValDictM)
  -- HSChain instances
  deriving MonadTMMonitoring      via NoMonitoring   (KeyValT m)
  deriving MonadLogger            via LoggerByTypes  (KeyValT m)
  deriving (MonadReadDB, MonadDB) via DatabaseByType (KeyValT m)
  deriving (MonadCached BData)    via CachedByType BData (KeyValT m)

instance MonadTrans KeyValT where
  lift = KeyValT . lift

runKeyValT :: KeyValDictM -> KeyValT m a -> m a
runKeyValT d = flip runReaderT d . unKeyValT


executeSpec
  :: ()
  => MockClusterConfig BData ()
  -> AppCallbacks (KeyValT IO) BData
  -> ContT r IO [(KeyValState, KeyValDictM)]
executeSpec MockClusterConfig{..} callbacks = do
  -- Create mock network and allocate DB handles for nodes
  net       <- liftIO P2P.newMockNet
  resources <- allocNetwork net clusterTopology clusterNodes
  rnodes    <- lift $ forM resources $ \(spec, bnet, dictConn, dictLogEnv) -> do
    dictCached <- newCached
    let dict = KeyValDictM { dictNamespace = mempty
                           , ..
                           }
    (state, threads) <- runKeyValT dict $ interpretSpec
      genesis
      spec
      bnet
      clusterCfg
      callbacks
    return (state,dict,threads)
  --
  lift $ catchAbort $ runConcurrently $ do (_,dict,thread) <- rnodes
                                           runKeyValT dict <$> thread
  return [ (s,d) | (s,d,_) <- rnodes ]
  where
    valSet  = makeValidatorSetFromPriv $ catMaybes [ nspecPrivKey x | x <- clusterNodes ]
    genesis = mkGenesisBlock valSet
