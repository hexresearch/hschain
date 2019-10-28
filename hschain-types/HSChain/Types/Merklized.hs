{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# LANGUAGE TypeApplications      #-}
-- |
-- Type classes for working with heterogenoeus merkle trees that is
-- trees which can contain values of different types.
module HSChain.Types.Merklized where

import qualified Codec.Serialise as CBOR
import qualified Data.Aeson      as JSON
import HSChain.Crypto

-- | Node of merkle tree. This data type has special 'CryptoHashable'
--   instance which just passes raw hash bytestring to the hash
--   function. It's sole function is for implementing Merkle trees.
data Merkled alg a = Merkled
  { merkleHash  :: !(Hash alg)
  , merkleValue :: !a
  }
  deriving (Show,Eq,Ord,Functor,Foldable)

instance (CryptoHash alg, CryptoHashable a, CBOR.Serialise a
         ) => CBOR.Serialise (Merkled alg a) where
  decode = merkled <$> CBOR.decode
  encode = CBOR.encode . merkleValue

instance (JSON.ToJSON a) => JSON.ToJSON (Merkled alg a) where
  toJSON = JSON.toJSON . merkleValue

instance (CryptoHash alg, CryptoHashable a, JSON.FromJSON a
         ) => JSON.FromJSON (Merkled alg a) where
  parseJSON = fmap merkled . JSON.parseJSON


merkled :: (CryptoHash alg, CryptoHashable a) => a -> Merkled alg a
merkled a = Merkled (hash a) a

checkMerkled :: (CryptoHash alg, CryptoHashable a) => Merkled alg a -> Bool
checkMerkled n = merkleHash n == hash (merkleValue n)

instance CryptoHashable (Merkled alg a) where
  hashStep s (Merkled (Hash h) _) = updateHashAccum s h
