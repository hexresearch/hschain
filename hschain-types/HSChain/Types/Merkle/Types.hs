{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- Type classes for working with heterogenoeus merkle trees that is
-- trees which can contain values of different types.
module HSChain.Types.Merkle.Types (
    -- * Type classes
    MerkleHash(..)
  , IsMerkle(..)
    -- * Merkle node
  , MerkleNode(..)
    -- ** Concrete node instantiations
  , Hashed(..)
  , OptNode
  , IdNode
  , merkleValue
  , merkleHash
  , merkleMaybeValue
    -- * Serialization helpers
  , MerkleSerialize(..)
  , toSerialisedRepr
  , fromSerializedRepr
  ) where

import Control.DeepSeq
import qualified Codec.Serialise as CBOR
import qualified Data.Aeson      as JSON
import Data.Functor.Classes
import Data.Function
import GHC.Generics (Generic)

import HSChain.Crypto


----------------------------------------------------------------
-- Heterogeneous Merkle trees
----------------------------------------------------------------

-- | Value identified by hash. It's expected that 'CryptoHashable' is
--   compatible with this instance:
--
-- > hash a == hash (merkleHash a)
class MerkleHash f where
  -- | Obtain cached hash of node.
  merkleHashed :: f alg a -> Hashed alg a

-- | Type class for nodes of Merkle tree. It contains operations that
--   are common for all variants of nodes whether they contain actual
--   value or not.
class MerkleHash f => IsMerkle f where
  -- | Create node from bare value
  merkled     :: (CryptoHash alg, CryptoHashable a) => a -> f alg a
  -- | Convert to node with optional value
  toOptNode   :: f alg a -> OptNode alg a
  -- | Convert node with optional value to hash
  fromOptNode :: OptNode alg a -> Maybe (f alg a)


-- | Newtype wrapper for nodes of Merkle trees. It's used to provide
--   necessary instances and to
--
-- > MerkleNode IdNode   - node that always have value
-- > MerkleNode OptNode  - node that optionally has hash in it
-- > MerkleNode Hashed   - node that hash only hash
newtype MerkleNode f alg a = MerkleNode { getMerkleNode :: f alg a }
  deriving stock   Foldable
  deriving newtype (MerkleHash, IsMerkle)

-- | Extract value from node of Merkle tree
merkleValue :: MerkleNode IdNode alg a -> a
merkleValue (MerkleNode (IdNode _ a)) = a

merkleMaybeValue :: IsMerkle f => f alg a -> Maybe a
merkleMaybeValue n = let OptNode _ a = toOptNode n in a

merkleHash :: MerkleHash f => f alg a -> Hash alg
merkleHash f = let Hashed h = merkleHashed f in h

-- | Eq uses hash comparison as optimization
instance (IsMerkle f) => Eq (MerkleNode f alg a) where
  (==) = (==) `on` merkleHash . getMerkleNode

-- | Ord however compares underlying types
instance (Ord a, IsMerkle f, Ord1 (f alg)) => Ord (MerkleNode f alg a) where
  compare = liftCompare compare `on` getMerkleNode

instance (Show a, Show1 (f alg)) => Show (MerkleNode f alg a) where
  showsPrec i = liftShowsPrec showsPrec showList i . getMerkleNode

instance (NFData a, NFData1 (f alg)) => NFData (MerkleNode f alg a) where
  rnf = liftRnf rnf . getMerkleNode

instance (CryptoHash alg, IsMerkle f) => CryptoHashable (MerkleNode f alg a) where
  hashStep = hashStep . merkleHash . getMerkleNode


----------------------------------------------------------------
-- Types for nodes
----------------------------------------------------------------


data IdNode  alg a = IdNode  !(Hashed alg a) !a
  deriving stock (Show,Eq,Ord,Generic)

data OptNode alg a = OptNode !(Hashed alg a) !(Maybe a)
  deriving stock (Show,Eq,Ord,Generic)


instance MerkleHash IdNode where
  merkleHashed (IdNode h _) = h
instance IsMerkle IdNode where
  merkled a = IdNode (hashed a) a
  toOptNode   (IdNode  h a) = OptNode h (Just a)
  fromOptNode (OptNode h a) = IdNode  h <$> a

instance MerkleHash OptNode where
  merkleHashed (OptNode h _) = h
instance IsMerkle OptNode where
  merkled a = OptNode (hashed a) (Just a)
  toOptNode   = id
  fromOptNode = Just

instance MerkleHash Hashed where
  merkleHashed = id
instance IsMerkle Hashed where
  merkled = hashed
  toOptNode h = OptNode h Nothing
  fromOptNode = Just . Hashed . merkleHash

instance NFData1 (IdNode alg) where
  liftRnf f (IdNode h a) = rnf h `seq` f a

instance Show1 (IdNode alg) where
  liftShowsPrec sh _ i (IdNode h a)
    = showParen (i >= 11)
    $ showString "IdNode "
    . showParen True (shows h)
    . showChar ' '
    . sh 11 a

-- | We cheat by comparing only hashes
instance Eq1 (IdNode alg) where
  liftEq f (IdNode _ a) (IdNode _ b) = f a b

instance Ord1 (IdNode alg) where
  liftCompare f (IdNode _ a) (IdNode _ b) = f a b

----------------------------------------------------------------
-- Serialization instances
----------------------------------------------------------------

-- | Data type which is used to derive serializaion
data MerkleSerialize alg a
  = MerkleValue a
  | MerkleHash  (Hashed alg a)
  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (CBOR.Serialise, JSON.FromJSON, JSON.ToJSON)

toSerialisedRepr :: OptNode alg a -> MerkleSerialize alg a
toSerialisedRepr (OptNode h Nothing ) = MerkleHash  h
toSerialisedRepr (OptNode _ (Just a)) = MerkleValue a

fromSerializedRepr :: (CryptoHash alg, CryptoHashable a) => MerkleSerialize alg a -> OptNode alg a
fromSerializedRepr (MerkleHash  h) = OptNode h Nothing
fromSerializedRepr (MerkleValue a) = merkled a

instance ( CryptoHash alg
         , CryptoHashable a
         , IsMerkle f
         , CBOR.Serialise a
         ) => CBOR.Serialise (MerkleNode f alg a) where
  encode = CBOR.encode . toSerialisedRepr . toOptNode . getMerkleNode
  decode = do
    n <- CBOR.decode
    case fromOptNode $ fromSerializedRepr n of
      Nothing -> fail "Can't covert from optional node"
      Just a  -> return $ MerkleNode a

instance (JSON.ToJSON a, IsMerkle f) => JSON.ToJSON (MerkleNode f alg a) where
  toJSON = JSON.toJSON . toSerialisedRepr . toOptNode . getMerkleNode

instance ( CryptoHash alg
         , CryptoHashable a
         , IsMerkle f
         , JSON.FromJSON a
         ) => JSON.FromJSON (MerkleNode f alg a) where
  parseJSON o = do
    n <- JSON.parseJSON o
    case fromOptNode $ fromSerializedRepr n of
      Nothing -> fail "Can't covert from optional node"
      Just a  -> return $ MerkleNode a
