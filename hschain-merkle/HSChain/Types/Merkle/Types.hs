{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- Type classes for working with heterogenoeus merkle trees where we
-- can nodes which have attached hash and could be abbreviated to hash
-- only.
module HSChain.Types.Merkle.Types (
    -- * Type classes
    IsMerkle(..)
  , MerkleMap(..)
    -- * Node of Merkle tree
  , MerkleNode(..)
  , merkleValue
  , merkleHash
  , merkleMaybeValue
  , toHashedNode
    -- ** Concrete node instantiations
  , Hashed(..)
  , OptNode
  , IdNode
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

-- | Type class for nodes of Merkle tree. Basically it's hash of value
--   and value itself which could be absent. There're three cases:
--   value is guaranteed to be present ('IdNode'), mayb or mayb not be
--   present ('OptNode') or only hash ('Hashed').
--
--   These data types are not supposed to be used by itself instead
--   'MerkleNode' should be used.
--
--   Instances should have instance of 'CryptoHashable' and only use
--   hash of value for calculating hash of node:
--
--   > hash a == hash (merkleHash a)
class IsMerkle f where
  -- | Obtain cached hash of node.
  merkleHashed :: f alg a -> Hashed alg a
  -- | Create node from bare value
  merkled     :: (CryptoHash alg, CryptoHashable a) => a -> f alg a
  -- | Convert to node with optional value
  toOptNode   :: f alg a -> OptNode alg a
  -- | Convert node with optional value to hash
  fromOptNode :: OptNode alg a -> Maybe (f alg a)

class MerkleMap b where
  merkleMap :: IsMerkle f => (forall alg a. f alg a -> g alg a) -> b f -> b g

-- | Newtype wrapper for nodes of Merkle trees. It's used to provide
--   serialization instance and ensure that data encoded as one node
--   could be decoded as another node e.g. (IdNode → OptNode)
--
-- > MerkleNode IdNode   - node that always have value
-- > MerkleNode OptNode  - node that optionally has hash in it
-- > MerkleNode Hashed   - node that hash only hash
newtype MerkleNode f alg a = MerkleNode { getMerkleNode :: f alg a }
  deriving stock   Foldable
  deriving newtype (IsMerkle)

-- | Extract value from node of Merkle tree
merkleValue :: MerkleNode IdNode alg a -> a
merkleValue (MerkleNode (IdNode _ a)) = a

-- | Extract value from any type of node of Merkle tree.
merkleMaybeValue :: IsMerkle f => f alg a -> Maybe a
merkleMaybeValue n = let OptNode _ a = toOptNode n in a

-- | Extract hash corresponding to node
merkleHash :: IsMerkle f => f alg a -> Hash alg
merkleHash f = let Hashed h = merkleHashed f in h

toHashedNode :: IsMerkle f => MerkleNode f alg a -> MerkleNode Hashed alg a
toHashedNode (MerkleNode f) = MerkleNode $ merkleHashed f

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

-- | Node that contains actual value alognside with hash. It's
--   expected to be used primarily as type parameter to 'MerkleNode':
--   @MerkleNode IdNode@
data IdNode  alg a = IdNode  (Hashed alg a) !a
  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (NFData)

-- | Node that /may/ contain value alognside with hash. It's
--   expected to be used primarily as type parameter to 'MerkleNode':
--   @MerkleNode OptNode@
data OptNode alg a = OptNode (Hashed alg a) !(Maybe a)
  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (NFData)

instance IsMerkle IdNode where
  merkleHashed (IdNode h _) = h
  merkled a = IdNode (hashed a) a
  toOptNode   (IdNode  h a) = OptNode h (Just a)
  fromOptNode (OptNode h a) = IdNode  h <$> a

instance IsMerkle OptNode where
  merkleHashed (OptNode h _) = h
  merkled a = OptNode (hashed a) (Just a)
  toOptNode   = id
  fromOptNode = Just

instance IsMerkle Hashed where
  merkleHashed = id
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
