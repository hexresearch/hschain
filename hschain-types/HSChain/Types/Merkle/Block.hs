{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module HSChain.Types.Merkle.Block (
    -- * Tree data types
    MerkleRoot(..)
  , MerkleTree(..)
  , MerkleNode(..)
  , MerkleChild(..)
    -- * Building tree
  , merklize
    -- * Reassembly of tree
  , concatTree
  , checkMerkleTree
  ) where

import Codec.Serialise
import Data.Functor.Identity
import Data.Typeable
import Data.Word

import Data.ByteString (ByteString)
import GHC.Generics    (Generic)

import qualified Data.ByteString as BS

import HSChain.Crypto
import HSChain.Crypto.Classes.Hash


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

instance CryptoHash alg => CryptoHashable (MerkleTree alg f)  where
  hashStep = hashStep . merkleRoot
instance CryptoHash alg => CryptoHashable (MerkleRoot alg)    where
  hashStep = genericHashStep "hschain"
instance CryptoHash alg => CryptoHashable (MerkleNode alg f)  where
  hashStep node
    = hashStep (UserType "hschain" "Merkle.Block.Node")
   <> case node of
        Branch xs -> hashStep (ConstructorIdx 0)
                  <> hashStep (merkleNodeHash <$> xs)
        Leaf   bs -> hashStep (ConstructorIdx 1)
                  <> hashStep bs
instance CryptoHash alg => CryptoHashable (MerkleChild alg f) where
  hashStep = hashStep . merkleNodeHash

----------------------------------------------------------------
-- Build tree
----------------------------------------------------------------

merklize
  :: forall alg. CryptoHash alg
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
          , rootHash = hash root
          }
      , merkleTree = root
      }
  where
    root   = buildTree fanout leafs
    fanout = chunkSize `div` hashSize (Proxy @ alg)
    leafs  = Leaf <$> chunkBS (fromIntegral chunkSize) blob


buildTree :: (CryptoHash alg) => Word32 -> [MerkleNode alg Identity] -> MerkleNode alg Identity
buildTree _      []    = error "Cannot have empty list as parameter"
buildTree _      [n]   = n
buildTree fanout nodes = buildTree fanout
                       $ map (Branch . map makeChild)
                       $ chunk (fromIntegral fanout) nodes

makeChild :: (CryptoHash alg) => MerkleNode alg Identity -> MerkleChild alg Identity
makeChild node = MerkleChild
  { merkleNodeHash = hash node
  , merkleChild    = Identity node
  }

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
-- Assemble tree
----------------------------------------------------------------

concatTree :: CryptoHash alg => MerkleTree alg Identity -> Maybe ByteString
concatTree tree
  | checkMerkleTree tree = Just $ BS.concat $ recur $ merkleTree tree
  | otherwise            = Nothing
  where
    recur (Leaf   bs) = [bs]
    recur (Branch ns) = foldMap (recur . runIdentity . merkleChild) ns

checkMerkleTree :: CryptoHash alg => MerkleTree alg Identity -> Bool
checkMerkleTree MerkleTree{..}
  =  hash merkleTree == rootHash merkleRoot
  && checkNode  merkleTree
  && checkDepth 1 merkleTree
  where
    depth = treeDepth merkleRoot
    checkDepth n Leaf{}      = n == depth
    checkDepth n (Branch ns) = all (checkDepth (n+1) . runIdentity .  merkleChild) ns

    checkNode Leaf{}      = True
    checkNode (Branch ns) = all checkChild ns
    checkChild (MerkleChild h (Identity node))

      =  h == hash node
      && checkNode node


----------------------------------------------------------------
-- Deltas
----------------------------------------------------------------

{-
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
-}


----------------------------------------------------------------
--
----------------------------------------------------------------


numberLeaves :: MerkleRoot alg -> Word32
numberLeaves MerkleRoot{..}
  = max 1
  $ blobSize `div1` partSize

treeFanout :: forall alg. CryptoHash alg => MerkleRoot alg -> Word32
treeFanout MerkleRoot{..}
  = partSize `div` hashSize (Proxy @ alg)

treeDepth :: CryptoHash alg => MerkleRoot alg -> Word32
treeDepth root@MerkleRoot{..}
  | blobSize <= partSize = 1
  | otherwise            = 1 + dlog (treeFanout root) (numberLeaves root)

dlog :: Integral i => i -> i -> i
dlog base n
  | n <= base = 1
  | otherwise = 1 + dlog base (n `div1` base)


div1 :: Integral i => i -> i -> i
div1 n m = case n `divMod` m of
  (i,0) -> i
  (i,_) -> i+1
