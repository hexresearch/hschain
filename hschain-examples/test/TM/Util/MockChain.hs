{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Data for mock blockchain used in tests for consensus and gossip
module TM.Util.MockChain where

import Control.Monad.Catch
import Control.Monad.Reader
import           Data.List (sortOn)
import qualified Data.Map.Strict    as Map
import qualified Data.List.NonEmpty as NE

import HSChain.Control.Class
import HSChain.Crypto
import HSChain.Logger
import HSChain.Store
import HSChain.Monitoring
import HSChain.Types
import HSChain.Types.Merkle.Types
import HSChain.Internal.Types.Consensus
import HSChain.Mock.KeyList
import HSChain.Mock.KeyVal  (BData(..),mkGenesisBlock)


----------------------------------------------------------------
-- Monad for running tests
----------------------------------------------------------------

newtype HSChainT a m x = HSChainT (ReaderT (Connection 'RW a) m x)
  deriving newtype (Functor,Applicative,Monad,MonadIO,MonadFail)
  deriving newtype (MonadThrow,MonadCatch,MonadMask,MonadFork)
  deriving newtype (MonadReader (Connection 'RW a))
  -- HSChain instances
  deriving MonadTMMonitoring          via NoMonitoring       (HSChainT a m)
  deriving MonadLogger                via NoLogsT            (HSChainT a m)
  deriving (MonadReadDB a, MonadDB a) via DatabaseByReader a (HSChainT a m)

runHSChainT :: Connection 'RW a -> HSChainT a m x -> m x
runHSChainT c (HSChainT m) = runReaderT m c


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
