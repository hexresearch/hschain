{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
-- |
-- Type classes for working with heterogenoeus merkle trees that is
-- trees which can contain values of different types.
module HSChain.Types.Merklized where

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

-- | Type class for nodes of Merkle tree. It contains operations that
--   are common for all variants of nodes whether they contain actual
--   value or not.
class IsMerkle f where
  -- | Obtain cached hash of node.
  merkleHash  :: f alg a -> Hash alg
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
  deriving newtype IsMerkle


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

instance IsMerkle f => CryptoHashable (MerkleNode f alg a) where
  hashStep s a = let Hash bs = merkleHash $ getMerkleNode a
                 in updateHashAccum s bs


----------------------------------------------------------------
-- Types for nodes
----------------------------------------------------------------


data IdNode  alg a = IdNode  !(Hash alg) !a
  deriving stock (Show,Eq,Ord,Foldable,Generic)

data OptNode alg a = OptNode !(Hash alg) !(Maybe a)
  deriving stock (Show,Eq,Ord,Generic)

instance IsMerkle IdNode where
  merkleHash (IdNode h _) = h
  merkled a = IdNode (hash a) a
  toOptNode   (IdNode  h a) = OptNode h (Just a)
  fromOptNode (OptNode h a) = IdNode  h <$> a

instance IsMerkle OptNode where
  merkleHash (OptNode h _) = h
  merkled a = OptNode (hash a) (Just a)
  toOptNode   = id
  fromOptNode = Just

instance IsMerkle Hashed where
  merkleHash (Hashed h) = h
  merkled = hashed
  toOptNode (Hashed h) = OptNode h Nothing
  fromOptNode = Just . Hashed . merkleHash



----------------------------------------------------------------
-- Serialization instances
----------------------------------------------------------------

-- | Data type which is used to derive serializaion
data MerkleSerialize alg a
  = MerkleValue a
  | MerkleHash  (Hash alg)
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
