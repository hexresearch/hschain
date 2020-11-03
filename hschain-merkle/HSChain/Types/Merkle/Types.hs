{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- |
-- Type classes and data types for working with heterogenoeus merkle
-- trees where we can nodes which have attached hash and could be
-- abbreviated to hash only.
module HSChain.Types.Merkle.Types (
    -- * Type classes
    IsMerkle(..)
  , IsMerkleNode(..)
  , MerkleMap(..)
    -- * Node of Merkle tree
  , MerkleNode(..)
  , pattern MerkleHash
  , pattern MerkleHashed
  , pattern MerkleNode
    -- ** Construction
  , merkled
  , fromHashed
    -- ** Access
  , merkleHashed
  , merkleValue
  , merkleNodeValue
    -- * Reexports
  , Identity(..)
  , Proxy(..)
    -- * Serialization helpers
  , MerkleSerialize(..)
  , toSerialisedRepr
  , fromSerializedRepr
  ) where

import Control.DeepSeq
import qualified Codec.Serialise as CBOR
import qualified Data.Aeson      as JSON
import Data.Coerce
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Proxy
import GHC.Generics (Generic)

import HSChain.Crypto


----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | Type class for wrappers for nodes of Merkle tree. Basically
--   there're 3 instances: @Identity@, @Maybe@, and @Proxy@. They
--   represent possibilities that node is present, may be present, we
--   have only hash.
class Applicative f => IsMerkle f where
  nodeToMaybe   :: f a -> Maybe a
  nodeFromMaybe :: Maybe a -> Maybe (f a)

instance IsMerkle Maybe where
  nodeToMaybe   = id
  nodeFromMaybe = Just

instance IsMerkle Proxy where
  nodeToMaybe   _ = Nothing
  nodeFromMaybe _ = Just Proxy

instance IsMerkle Identity where
  nodeToMaybe   = Just . runIdentity
  nodeFromMaybe = coerce

-- | Type class for Merkle trees\/nodes.
class IsMerkleNode t where
  -- | Obtain hash corresponding to merkle node
  merkleHash :: t alg f a -> Hash alg
  -- | Change type of intermediate node
  mapMerkleNode :: IsMerkle g => (forall x. f x -> g x) -> t alg f a -> t alg g a
  -- | Erase all data in the node.
  toHashedNode :: t alg f a -> t alg Proxy a
  toHashedNode = mapMerkleNode (const Proxy)
  -- | Construct node with only root hash
  nodeFromHash :: Hash alg -> t alg Proxy a

class MerkleMap b where
  merkleMap :: IsMerkle g => (forall a. f a -> g a) -> b f -> b g


----------------------------------------------------------------
-- Heterogeneous Merkle trees
----------------------------------------------------------------

-- | Node of Merkle tree. It stores hash of node and node value as @f
--   a@ which could be present or missing depending on type of
--   @f@. Common parametrizations are:
--
-- > MerkleNode Identity - node that always have value
-- > MerkleNode Maybe    - node that optionally has hash in it
-- > MerkleNode Proxy    - node that has only hash
data MerkleNode alg f a = MNode
  -- NOTE: Hash is lazy on purpose. We want to avoid computing it when
  --       it's not needed. Since we store hash alongside with value
  --       it should not leak.
  (Hashed alg a)
  !(f a)

-- | Pattern match on merkle node and extract its hash
pattern MerkleHash :: Hash alg -> MerkleNode alg f a
pattern MerkleHash h <- MNode (Hashed h) _
{-# COMPLETE MerkleHash #-}

-- | Pattern match on merkle node and extract its hash with type tag
pattern MerkleHashed :: Hashed alg a -> MerkleNode alg f a
pattern MerkleHashed h <- MNode h _
{-# COMPLETE MerkleHashed #-}

-- | Pattern match on merkle node and extract both hash and value
pattern MerkleNode :: Hashed alg a -> a -> MerkleNode alg Identity a
pattern MerkleNode h a <- MNode h (Identity a)
{-# COMPLETE MerkleNode #-}

instance Foldable f => Foldable (MerkleNode alg f) where
  foldMap f (MNode _ a) = foldMap f a

-- | Eq uses hash comparison as optimization
instance (IsMerkle f) => Eq (MerkleNode alg f a) where
  MNode h1 _ == MNode h2 _ = h1 == h2

-- | Ord however compares underlying types
instance (Ord a) => Ord (MerkleNode alg Identity a) where
  compare (MNode _ f1) (MNode _ f2) = liftCompare compare f1 f2

instance (Show1 f, Show a) => Show (MerkleNode alg f a) where
  showsPrec i (MNode h f)
    = showParen (i >= 11)
    $ showString "MerkleNode "
    . showsPrec 11 h
    . showString " "
    . liftShowsPrec showsPrec showList 11 f

instance (NFData a, NFData1 f) => NFData (MerkleNode alg f a) where
  rnf (MNode h f) = rnf h `seq` liftRnf rnf f

instance (CryptoHash alg) => CryptoHashable (MerkleNode alg f a) where
  hashStep = hashStep . merkleHash

instance IsMerkleNode MerkleNode where
  merkleHash (MNode (Hashed h) _) = h
  mapMerkleNode f (MNode !h a) = MNode h (f a)
  -- NOTE: We force hash in order to avoid space leak. Hash field is
  --       lazy and could keep refernce to possibly much larger object
  toHashedNode (MNode !h _) = MNode h Proxy
  nodeFromHash h = MNode (Hashed h) Proxy

-- | Extract hash corresponding to node which is tagged by type.
merkleHashed :: MerkleNode alg f a -> Hashed alg a
merkleHashed (MNode h _) = h

-- | Extract value from node of Merkle tree
merkleValue :: MerkleNode alg Identity a -> a
merkleValue = runIdentity . merkleNodeValue

-- | Extract value from node of Merkle tree
merkleNodeValue :: MerkleNode alg f a -> f a
merkleNodeValue (MNode _ a) = a


-- | Create node from bare value. We (ab)ue
merkled :: (CryptoHash alg, CryptoHashable a, Applicative f) => a -> MerkleNode alg f a
merkled a = MNode (hashed a) (pure a)

-- | Create node from only value
fromHashed :: Hashed alg a -> MerkleNode alg Proxy a
fromHashed h = MNode h Proxy



----------------------------------------------------------------
-- Serialization instances
----------------------------------------------------------------

-- | Data type which is used to derive serializaion
data MerkleSerialize alg a
  = SMerkleValue a
  | SMerkleHash  (Hashed alg a)
  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (CBOR.Serialise, JSON.FromJSON, JSON.ToJSON)

toSerialisedRepr :: IsMerkle f => MerkleNode alg f a -> MerkleSerialize alg a
toSerialisedRepr (MNode h f) =
  case nodeToMaybe f of Nothing -> SMerkleHash  h
                        Just a  -> SMerkleValue a


fromSerializedRepr :: (CryptoHash alg, CryptoHashable a) => MerkleSerialize alg a -> MerkleNode alg Maybe a
fromSerializedRepr (SMerkleHash  h) = MNode h Nothing
fromSerializedRepr (SMerkleValue a) = MNode (hashed a) (Just a)

instance ( CryptoHash alg
         , CryptoHashable a
         , IsMerkle f
         , CBOR.Serialise a
         ) => CBOR.Serialise (MerkleNode alg f a) where
  encode = CBOR.encode . toSerialisedRepr . toMaybeNode
  decode = do
    MNode h mn <- fromSerializedRepr <$> CBOR.decode
    case nodeFromMaybe mn of
      Nothing -> fail "Can't covert from optional node"
      Just a  -> return $ MNode h a

instance (JSON.ToJSON a, IsMerkle f) => JSON.ToJSON (MerkleNode alg f a) where
  toJSON = JSON.toJSON . toSerialisedRepr . toMaybeNode

instance ( CryptoHash alg
         , CryptoHashable a
         , IsMerkle f
         , JSON.FromJSON a
         ) => JSON.FromJSON (MerkleNode alg f a) where
  parseJSON o = do
    MNode h mn <- fromSerializedRepr <$> JSON.parseJSON o
    case nodeFromMaybe mn of
      Nothing -> fail "Can't covert from optional node"
      Just a  -> return $ MNode h a

toMaybeNode :: IsMerkle f => MerkleNode alg f a -> MerkleNode alg Maybe a
toMaybeNode (MNode h f) = MNode h (nodeToMaybe f)
