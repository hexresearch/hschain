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
  , MerkleMap(..)
    -- * Node of Merkle tree
  , MerkleNode
  , pattern MerkleHash
  , pattern MerkleHashed
  , pattern MerkleNode
    -- ** Construction
  , merkled
  , fromHashed
    -- ** Access
  , merkleHash
  , merkleHashed
  , merkleValue
  , merkleNodeValue
  , mapMerkleNode
  , toHashedNode
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
-- Heterogeneous Merkle trees
----------------------------------------------------------------

-- | Node of Merkle tree. It stores hash of node and node value as @f
--   a@ which could be present or missing depending on type of
--   @f@. Common parametrizations are:
--
-- > MerkleNode Identity - node that always have value
-- > MerkleNode Maybe    - node that optionally has hash in it
-- > MerkleNode Proxy    - node that has only hash
data MerkleNode f alg a = MNode
  -- NOTE: Hash is lazy on purpose. We want to avoid computing it when
  --       it's not needed. Since we store hash alongside with value
  --       it should not leak.
  (Hashed alg a)
  !(f a)

-- | Pattern match on merkle node and extract its hash
pattern MerkleHash :: Hash alg -> MerkleNode f alg a
pattern MerkleHash h <- MNode (Hashed h) _
{-# COMPLETE MerkleHash #-}

-- | Pattern match on merkle node and extract its hash with type tag
pattern MerkleHashed :: Hashed alg a -> MerkleNode f alg a
pattern MerkleHashed h <- MNode h _
{-# COMPLETE MerkleHashed #-}

-- | Pattern match on merkle node and extract both hash and value
pattern MerkleNode :: Hashed alg a -> a -> MerkleNode Identity alg a
pattern MerkleNode h a <- MNode h (Identity a)
{-# COMPLETE MerkleNode #-}

instance Foldable f => Foldable (MerkleNode f alg) where
  foldMap f (MNode _ a) = foldMap f a

-- | Eq uses hash comparison as optimization
instance (IsMerkle f) => Eq (MerkleNode f alg a) where
  MNode h1 _ == MNode h2 _ = h1 == h2

-- | Ord however compares underlying types
instance (Ord a) => Ord (MerkleNode Identity alg a) where
  compare (MNode _ f1) (MNode _ f2) = liftCompare compare f1 f2

instance (Show1 f, Show a) => Show (MerkleNode f alg a) where
  showsPrec i (MNode h f)
    = showParen (i >= 11)
    $ showString "MerkleNode "
    . showsPrec 11 h
    . showString " "
    . liftShowsPrec showsPrec showList 11 f

instance (NFData a, NFData1 f) => NFData (MerkleNode f alg a) where
  rnf (MNode h f) = rnf h `seq` liftRnf rnf f

instance (CryptoHash alg, IsMerkle f) => CryptoHashable (MerkleNode f alg a) where  
  hashStep = hashStep . merkleHash


-- | Extract hash corresponding to node
merkleHash :: MerkleNode f alg a -> Hash alg
merkleHash (MNode (Hashed h) _) = h

-- | Extract hash corresponding to node which is tagged by type.
merkleHashed :: MerkleNode f alg a -> Hashed alg a
merkleHashed (MNode h _) = h

-- | Extract value from node of Merkle tree
merkleValue :: MerkleNode Identity alg a -> a
merkleValue = runIdentity . merkleNodeValue

-- | Extract value from node of Merkle tree
merkleNodeValue :: MerkleNode f alg a -> f a
merkleNodeValue (MNode _ a) = a


-- | Create node from bare value. We (ab)ue 
merkled :: (CryptoHash alg, CryptoHashable a, Applicative f) => a -> MerkleNode f alg a
merkled a = MNode (hashed a) (pure a)

-- | Create node from only value 
fromHashed :: Hashed alg a -> MerkleNode Proxy alg a
fromHashed h = MNode h Proxy


-- | Apply natural transformation to the node value. Value couldn't be
--   changed by transformation because of prametricity
mapMerkleNode :: (forall x. f x -> g x) -> MerkleNode f alg a -> MerkleNode g alg a
-- NOTE: See note in toHashedNode
mapMerkleNode f (MNode !h a) = MNode h (f a)

-- | Strip value from node
toHashedNode :: MerkleNode f alg a -> MerkleNode Proxy alg a
-- NOTE: We force hash in order to avoid space leak. Hash field is
--       lazy and could keep refernce to possibly much larger object
toHashedNode (MNode !h _) = MNode h Proxy


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

class MerkleMap b where
  merkleMap :: IsMerkle f => (forall a. f a -> g a) -> b f -> b g



----------------------------------------------------------------
-- Serialization instances
----------------------------------------------------------------

-- | Data type which is used to derive serializaion
data MerkleSerialize alg a
  = SMerkleValue a
  | SMerkleHash  (Hashed alg a)
  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (CBOR.Serialise, JSON.FromJSON, JSON.ToJSON)

toSerialisedRepr :: IsMerkle f => MerkleNode f alg a -> MerkleSerialize alg a
toSerialisedRepr (MNode h f) =
  case nodeToMaybe f of Nothing -> SMerkleHash  h
                        Just a  -> SMerkleValue a


fromSerializedRepr :: (CryptoHash alg, CryptoHashable a) => MerkleSerialize alg a -> MerkleNode Maybe alg a
fromSerializedRepr (SMerkleHash  h) = MNode h Nothing
fromSerializedRepr (SMerkleValue a) = MNode (hashed a) (Just a)

instance ( CryptoHash alg
         , CryptoHashable a
         , IsMerkle f
         , CBOR.Serialise a
         ) => CBOR.Serialise (MerkleNode f alg a) where
  encode = CBOR.encode . toSerialisedRepr . toMaybeNode
  decode = do
    MNode h mn <- fromSerializedRepr <$> CBOR.decode
    case nodeFromMaybe mn of
      Nothing -> fail "Can't covert from optional node"
      Just a  -> return $ MNode h a

instance (JSON.ToJSON a, IsMerkle f) => JSON.ToJSON (MerkleNode f alg a) where
  toJSON = JSON.toJSON . toSerialisedRepr . toMaybeNode

instance ( CryptoHash alg
         , CryptoHashable a
         , IsMerkle f
         , JSON.FromJSON a
         ) => JSON.FromJSON (MerkleNode f alg a) where
  parseJSON o = do
    MNode h mn <- fromSerializedRepr <$> JSON.parseJSON o
    case nodeFromMaybe mn of
      Nothing -> fail "Can't covert from optional node"
      Just a  -> return $ MNode h a

toMaybeNode :: IsMerkle f => MerkleNode f alg a -> MerkleNode Maybe alg a
toMaybeNode (MNode h f) = MNode h (nodeToMaybe f)
