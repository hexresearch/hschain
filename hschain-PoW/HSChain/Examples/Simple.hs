{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Simple block which implements write only key-value storage. It's
-- only use is to test and debug PoW algorithms.
module HSChain.Examples.Simple
  ( KV(..)
  , KVConfig(..)
  , KVState
  , retarget
  , kvMemoryView
  , createCandidateBlockData
  , hash256AsTarget
    -- * Monad
  , KVT(..)
  , runKVT
  ) where

import Codec.Serialise      (Serialise)
import Control.Applicative
import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Typeable        (Typeable)
import Data.Functor.Classes (Show1)
import Data.List            (find)
import qualified Data.Aeson           as JSON
import qualified Data.Map.Strict      as Map
import Katip (LogEnv, Namespace)
import GHC.Generics         (Generic)

import HSChain.Control.Class
import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.SHA
import HSChain.Logger
import HSChain.Types.Merkle.Types
import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.Store.Query


----------------------------------------------------------------
-- Monad for running Coin
----------------------------------------------------------------

data KVDict = KVDict
  { dictLogEnv    :: !LogEnv
  , dictNamespace :: !Namespace
  , dictConn      :: !(Connection 'RW)
  }
  deriving (Generic)

newtype KVT m a = KVT (ReaderT KVDict m a)
  deriving newtype ( Functor,Applicative,Monad,MonadIO
                   , MonadCatch,MonadThrow,MonadMask,MonadFork)
  deriving (MonadLogger)          via LoggerByTypes  (ReaderT KVDict m)
  deriving (MonadDB, MonadReadDB) via DatabaseByType (ReaderT KVDict m)

runKVT :: LogEnv -> Connection 'RW -> KVT m a -> m a
runKVT logenv conn (KVT act) = runReaderT act (KVDict logenv mempty conn)

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Simple block which contains key-value pairs. Work function is
--   simple SHA256 a la bitcoin
data KV cfg f = KV
  { kvData       :: !(MerkleNode SHA256 f [(Int,String)])
  -- ^ List of key-value pairs
  , kvTarget     :: !Target
  -- ^ Current difficulty of mining. It means a complicated thing
  -- right now.
  , kvNonce      :: !(Nonce cfg)
  -- ^ Nonce which is used to get
  }
  deriving stock (Generic)
deriving stock instance (Show (Nonce cfg), Show1 f)  => Show (KV cfg f)
deriving stock instance (Eq (Nonce cfg), IsMerkle f) => Eq   (KV cfg f)
instance Serialise (Nonce cfg) => Serialise (KV cfg Identity)
instance Serialise (Nonce cfg) => Serialise (KV cfg Proxy)

instance (CryptoHashable (Nonce cfg)) => CryptoHashable (KV cfg f) where
  hashStep = genericHashStep "hschain"

instance MerkleMap (KV cfg) where
  merkleMap f KV{..} = KV { kvData = mapMerkleNode f kvData
                          , ..
                          }

-- | We may need multiple chains (main chain, test chain(s)) which may
--   use different difficulty adjustment algorithms etc.
class ( CryptoHashable (Nonce cfg)
      , Serialise (Nonce cfg)
      , Typeable cfg
      ) => KVConfig cfg where
  -- | Type of nonce. It depends on configuration.
  type Nonce cfg

  kvDefaultNonce :: Proxy cfg -> Nonce cfg
  -- | Difficulty adjustment is performed every N of blocks
  kvAdjustInterval :: Const Height  cfg
  -- | Expected interval between blocks in milliseconds
  kvBlockTimeInterval  :: Const DTime cfg

  -- |How to compute a solved puzzle. May fail.
  kvSolvePuzzle :: MonadIO m => Block (KV cfg) -> m (Maybe (Block (KV cfg)))

  -- |How to check solution of a puzzle.
  kvCheckPuzzle :: MonadIO m => Header (KV cfg) -> m Bool


data KVError = KVError
  deriving stock    (Generic, Eq, Show)
  deriving anyclass (Exception, JSON.ToJSON)


instance (KVConfig cfg) => BlockData (KV cfg) where
  newtype BlockID (KV cfg) = KV'BID (Hash SHA256)
    deriving newtype (Show,Eq,Ord,CryptoHashable,Serialise, JSON.ToJSON, JSON.FromJSON)

  newtype TxID (KV cfg) = KV'TID (Hash SHA256)
    deriving newtype (Show,Eq,Ord,CryptoHashable,Serialise, JSON.ToJSON, JSON.FromJSON)

  type BlockException (KV cfg) = KVError
  type Tx             (KV cfg) = (Int, String)

  blockID = KV'BID . hash
  txID    = KV'TID . hash
  blockTransactions = merkleValue . kvData . blockData

  validateTxContextFree _ = Right ()

  validateHeader bh (Time now) header
     -- skip genesis check.
    | blockHeight header == 0 = return $ Right ()
    | otherwise = do
        answerIsGood <- kvCheckPuzzle header
        let ok = and
                   [ answerIsGood
                   , kvTarget (blockData header) == retarget bh
                   -- Time checks
                   , t <= now + (2*60*60*1000)
                   -- FIXME: Check that we're ahead of median time of N prev block
                   ]
        return $ if ok then Right () else Left KVError
    where
      Time t = blockTime header
  --
  validateBlock  _ = return $ Right ()
  blockWork      b = Work $ fromIntegral $ ((2^(256 :: Int)) `div`)
                          $ targetInteger $ kvTarget $ blockData b
  blockTargetThreshold b = Target $ targetInteger (kvTarget (blockData b))
  targetAdjustmentInfo (_ :: BH (KV cfg)) = (adjustInterval, blockMineTime)
    where
      Const adjustInterval = kvAdjustInterval :: Const Height cfg
      Const blockMineTime = kvBlockTimeInterval :: Const DTime cfg

instance (KVConfig cfg) => Mineable (KV cfg) where
  adjustPuzzle = fmap (flip (,) (Target 0)) . kvSolvePuzzle


data KVState cfg (m :: * -> *) = KVState
  { kvstBH    :: BH (KV cfg)
  , kvstPrev  :: KVState cfg m
  , kvstState :: Map.Map Int String
  }

instance (Monad m, KVConfig cfg) => StateView (KVState cfg m) where
  type BlockType (KVState cfg m) = (KV cfg)
  type MonadOf   (KVState cfg m) = m
  stateBH     = kvstBH
  revertBlock = pure . kvstPrev
  flushState  = pure
  --
  applyBlock view0@KVState{..} _ bh b = pure $ do
    st' <- kvViewStep b kvstState
    return KVState { kvstBH    = bh
                   , kvstPrev  = view0
                   , kvstState = st'
                   }
  --
  checkTx KVState{..} (k,_)
    | k `Map.notMember` kvstState = pure $ Right ()
    | otherwise                   = pure $ Left KVError
  
kvMemoryView :: forall m cfg. BH (KV cfg) -> KVState cfg m
kvMemoryView bh = KVState
  { kvstBH    = bh
  , kvstPrev  = error "No revinding past genesis"
  , kvstState = mempty
  }

createCandidateBlockData
  :: forall cfg m b. (BlockData b, KVConfig cfg)
  => KVState cfg m -> BH b -> [(Int, String)] -> KV cfg Identity
createCandidateBlockData KVState{..} bh txs = KV
  { kvData   = merkled $ case find ((`Map.notMember` kvstState) . fst) txs of
      Just tx -> [tx]
      Nothing -> []
  , kvNonce  = kvDefaultNonce (Proxy @cfg)
  , kvTarget = retarget bh
  }

kvViewStep :: Block (KV cfg) -> Map.Map Int String -> Either KVError (Map.Map Int String)
kvViewStep b m
  | or [ k `Map.member` m | (k, _) <- txs ] = Left KVError
  | otherwise                               = pure $ Map.fromList txs <> m
  where
    txs = merkleValue $ kvData $ blockData b
