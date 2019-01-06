{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module Thundermint.Types.Merkle (
    -- * Tree data types
    MerkleRoot(..)
  , MerkleTree(..)
  , MerkleNode(..)
  , MerkleChild(..)
    -- * Building tree
  , merklize
  ) where

import Codec.Serialise
import Data.Functor.Identity
import Data.Word
import Data.Typeable
import           Data.ByteString   (ByteString)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)

import Thundermint.Crypto


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Root of Merkle tree. It contains hash of root node and parameters
--   of tree.
data MerkleRoot alg = MerkleRoot 
  { blobSize :: !Word32
  , partSize :: !Word32
  , rootHash :: !(Hash alg)
  }
  deriving (Show, Eq, Generic)
instance Serialise (MerkleRoot alg)

-- | Complete tree. Type parameter @f@ describes whether tree is
--   complete @Identity@ or only partially constructed @Maybe@
data MerkleTree alg f = MerkleTree
  { merkleRoot :: !(MerkleRoot alg)
  , merkleTree :: !(MerkleNode alg f)
  }
  deriving (Generic)
deriving instance (Show (f (MerkleNode alg f))) => Show (MerkleTree alg f)
-- deriving instance (Eq   (f [MerkleNode alg f])) => Eq   (MerkleTree alg f)
-- instance Serialise (f [MerkleNode alg f]) => Serialise (MerkleTree alg f)


-- | Single node of tree
data MerkleNode alg f
  = Branch ![MerkleChild alg f]
  -- ^ List of leaf nodes of 
  | Leaf   !ByteString
  -- ^ Leaf node
  deriving (Generic)
deriving instance (Show (MerkleChild alg f)) => Show (MerkleNode alg f)

data MerkleChild alg f = MerkleChild
  { merkleNodeHash :: !(Hash alg)
  , merkleChild    :: !(f (MerkleNode alg f))
  }
  deriving (Generic)
deriving instance (Show (f (MerkleNode alg f))) => Show (MerkleChild alg f)
-- deriving instance (Eq   (f (MerkleNode alg f))) => Eq   (MerkleChild alg f)
-- instance Serialise (f (MerkleNode alg f)) => Serialise (MerkleChild alg f)


----------------------------------------------------------------
-- Build tree
----------------------------------------------------------------

merklize
  :: forall alg. Crypto alg
  => Word32                     -- ^ Size of chunk
  -> ByteString                 -- ^ Blob to split
  -> MerkleTree alg Identity
merklize chunkSize blob
  | chunkSize < 2 * hashSize (Proxy @ alg)
    = error "Chunk size is too small"
  | otherwise = MerkleTree
      { merkleRoot = MerkleRoot
          { blobSize = fromIntegral $ BS.length blob
          , partSize = chunkSize
          , rootHash = calculateNodeHash root
          }
      , merkleTree = root
      }
  where
    root   = buildTree fanout leafs
    fanout = chunkSize `div` hashSize (Proxy @ alg)    
    leafs  = Leaf <$> chunkBS (fromIntegral chunkSize) blob


buildTree :: (Crypto alg) => Word32 -> [MerkleNode alg Identity] -> MerkleNode alg Identity
buildTree _      []    = error "Cannot have empty list as parameter"
buildTree _      [n]   = n
buildTree fanout nodes = buildTree fanout
                       $ map (Branch . map makeChild)
                       $ chunk (fromIntegral fanout) nodes

makeChild :: (Crypto alg) => MerkleNode alg Identity -> MerkleChild alg Identity
makeChild node = MerkleChild
  { merkleNodeHash = calculateNodeHash node
  , merkleChild    = Identity node
  }

calculateNodeHash :: Crypto alg => MerkleNode alg Identity -> Hash alg
calculateNodeHash = \case
  Leaf   bs -> hash (0::Int, bs)
  Branch xs -> hash (1::Int, merkleNodeHash <$> xs)

data HashWrapper alg
  = HashBranch [Hash alg]
  | HashLeaf  !ByteString
  deriving (Generic)
instance Serialise (HashWrapper alg)

chunkBS :: Int -> ByteString -> [ByteString]
chunkBS n blob
  | BS.null bs' = [bs]
  | otherwise   = bs : chunkBS n bs'
  where
    (bs,bs') = BS.splitAt n blob

chunk :: Int -> [a] -> [[a]]
chunk n xs = case splitAt n xs of
  (c,[]) -> [c]
  (c,cs) -> c : chunk n cs

  
----------------------------------------------------------------
-- Deltas
----------------------------------------------------------------

data UpdLeaf alg = UpdLeaf
  { updRootL :: Hash alg
  , updLeafN :: Int
  , updLeaf  :: ByteString
  }

data UpdTree alg = UpdTree
  { updRootT  :: Hash alg
  , hashDepth :: Int
  , hashNum   :: Int
  , hashList  :: [Hash alg]
  }



----------------------------------------------------------------
--
----------------------------------------------------------------


nLeafs :: MerkleRoot alg -> Int
nLeafs = undefined

leafSize :: MerkleRoot alg -> Int -> Int
leafSize = undefined

treeDepth :: MerkleRoot alg -> Int
treeDepth = undefined
