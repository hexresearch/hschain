{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
-- |
-- Type classes for working with heterogenoeus merkle trees that is
-- trees which can contain values of different types.
module HSChain.Types.Merklized (
    -- * Type classes
    MerkleValue(..)
  ) where

import HSChain.Crypto

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Type for which we can compute hash. Therefore it could be put
--   into Merkle tree.
class CryptoHash alg => MerkleValue alg a where
  -- | Compute hash of value
  merkleHash :: a -> Hash alg

instance MerkleValue alg a => MerkleValue alg (Maybe a) where
  merkleHash = hash . fmap (merkleHash @alg)

instance MerkleValue alg a => MerkleValue alg [a] where
  merkleHash = hash . fmap (merkleHash @alg)
