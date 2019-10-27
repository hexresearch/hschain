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

import HSChain.Crypto

-- | Node of merkle tree. This data type has special 'CryptoHashable'
--   instance which just passes raw hash bytestring to the hash
--   function. It's sole function is for implementing Merkle trees.
data Merkled alg a = Merkled
  { merkleHash  :: !(Hash alg)
  , merkleValue :: !a
  }
  deriving (Show,Eq,Ord,Functor,Foldable)

merkled :: (CryptoHash alg, CryptoHashable a) => a -> Merkled alg a
merkled a = Merkled (hash a) a

checkMerkled :: (CryptoHash alg, CryptoHashable a) => Merkled alg a -> Bool
checkMerkled n = merkleHash n == hash (merkleValue n)

instance CryptoHashable (Merkled alg a) where
  hashStep s (Merkled (Hash h) _) = updateHashAccum s h
