{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Data for mock blockchain used in tests for consensus and gossip
module TM.Util.MockChain where

import Control.Monad.Catch
import Control.Monad.Reader
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import           Data.List (sortOn)
import qualified Data.Map.Strict    as Map
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)

import HSChain.Control.Class
import HSChain.Crypto
import HSChain.Logger
import HSChain.Store
import HSChain.Monitoring
import HSChain.Types
import HSChain.Types.Merkle.Types
import HSChain.Internal.Types.Consensus
import qualified HSChain.Store.Query as DB
import HSChain.Mock.KeyList
import HSChain.Mock.KeyVal  (BData(..),mkGenesisBlock)
import HSChain.Mock.Coin    (CoinStatements, CoinSpecification(..),MonadCoinDB(..))
import qualified HSChain.Mock.Coin as Coin

----------------------------------------------------------------
-- Monad for running tests
----------------------------------------------------------------

class InitExtra a where
  data family Extra a 
  initExtra :: (MonadIO m, MonadMask m, MonadDB m) => m (Extra a)
instance InitExtra BData where
  data instance Extra BData = ExtraKV
  initExtra = pure ExtraKV
instance InitExtra Coin.BData where
  data instance Extra Coin.BData = ExtraCoin CoinStatements
  initExtra = do
    DB.mustQueryRW Coin.initCoinDB
    ExtraCoin <$> Coin.newCoinStatements

data HSDict a = HSDict
  { dictConn  :: Connection 'RW
  , dictCache :: Cached a
  , dictExtra :: Extra  a
  }
  deriving Generic

newtype HSChainT a m x = HSChainT (ReaderT (HSDict a) m x)
  deriving newtype (Functor,Applicative,Monad,MonadIO,MonadFail)
  deriving newtype (MonadThrow,MonadCatch,MonadMask,MonadFork)
  deriving newtype (MonadReader (HSDict a))
  -- HSChain instances
  deriving MonadTMMonitoring      via NoMonitoring   (HSChainT a m)
  deriving MonadLogger            via NoLogsT        (HSChainT a m)
  deriving (MonadReadDB, MonadDB) via DatabaseByType (HSChainT a m)
  deriving (MonadCached a)        via CachedByType a (HSChainT a m)

runHSChainT :: (InitExtra a, MonadMask m, MonadIO m) => Connection 'RW -> HSChainT a m x -> m x
runHSChainT dictConn (HSChainT m) = do
  dictCache <- newCached
  dictExtra <- DB.runDBT dictConn $ do
    DB.mustQueryRW initializeBlockhainTables
    initExtra
  runReaderT m HSDict{..}

withHSChainT :: (MonadIO m, MonadMask m, InitExtra a) => HSChainT a m x -> m x
withHSChainT m = withDatabase "" $ \c -> do
  runHSChainT c m

instance MonadIO m => MonadCoinDB (HSChainT Coin.BData m) where
  coinStatements = do ExtraCoin stmt <- asks dictExtra
                      pure stmt

----------------------------------------------------------------
-- Validators for mockchain
----------------------------------------------------------------

privK       :: [PrivKey (Alg BData)]
k1,k2,k3,k4 :: PrivKey (Alg BData)
privK@[k1,k2,k3,k4] = sortOn publicKey $ take 4 $ makePrivKeyStream 1337

valSet :: ValidatorSet (Alg BData)
Right valSet = makeValidatorSet [Validator (publicKey k) 1 | k <- privK]


----------------------------------------------------------------
-- Mockchain itself
----------------------------------------------------------------

-- | Genesis block of BCh
genesis :: Genesis BData
genesis = mkGenesisBlock valSet

block1, block1' :: Block BData
block1  = mintFirstBlock $ BData [("K1",100)]
block1' = mintFirstBlock $ BData [("K1",101)]

mockchain :: [Block BData]
mockchain
  = fmap fst
  $ scanl step (genesisBlock genesis,mempty)
    [BData [("K"++show i,i)] | i <- [100..]]
  where
    step (b,st) dat@(BData txs) = let st' = st <> Map.fromList txs
                                  in ( mintBlock b dat, st' )


----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

mintFirstBlock :: BData -> Block BData
mintFirstBlock dat = mintBlock (genesisBlock genesis) dat

mintBlock :: Block BData -> BData -> Block BData
mintBlock b dat = Block
  { blockHeight        = succ hPrev
  , blockPrevBlockID   = Just bid
  , blockValidators    = hashed valSet
  , blockNewValidators = hashed valSet
  , blockData          = merkled dat
  , blockPrevCommit    = merkled <$> commit
  , blockEvidence      = merkled []
  }
  where
    hPrev  = blockHeight b
    r      = Round 0
    bid    = blockHash b
    commit | hPrev == Height 0 = Nothing
           | otherwise         = Just Commit
             { commitBlockID    = bid
             , commitPrecommits = NE.fromList
                 [ signValue i k $ Vote hPrev r (Time 0) (Just bid)
                 | k <- privK
                 , let Just i = indexByValidator valSet (publicKey k)
                 ]
             }

coinSpec :: CoinSpecification
coinSpec = CoinSpecification
  { coinAirdrop        = 1000
  , coinWallets        = 1000
  , coinWalletsSeed    = 1337
  , coinGeneratorDelay = Just 200
  , coinMaxMempoolSize = 1000
  , coinMaxBlockSize   = 1000
  }
