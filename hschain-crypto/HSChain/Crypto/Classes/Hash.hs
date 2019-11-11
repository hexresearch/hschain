{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-- |
module HSChain.Crypto.Classes.Hash where

import           Codec.Serialise   (Serialise, serialise)
import qualified Codec.Serialise as CBOR
import Control.Applicative
import Control.Monad.ST
import Control.Monad
import Control.DeepSeq

import qualified Data.Aeson               as JSON
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BC8
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import qualified Data.List.NonEmpty       as NE
import Data.Functor.Classes
import Data.Word
import Data.Proxy
import Data.Int
import Foreign.Ptr          (castPtr)
import Foreign.Storable     (Storable(..))
import System.IO.Unsafe
import Text.Read
import Text.ParserCombinators.ReadP

import GHC.TypeNats
import GHC.Generics hiding (Constructor)
import qualified GHC.Generics as GHC

import HSChain.Crypto.Classes


----------------------------------------------------------------
-- Cryptographic hashes
----------------------------------------------------------------

-- | Cryptographic hash of some value
newtype Hash alg = Hash BS.ByteString
  deriving stock   (Generic, Generic1)
  deriving newtype (Eq,Ord,Serialise,NFData)

-- | Newtype wrapper with phantom type tag which show hash of which
--   value is being calculated
newtype Hashed alg a = Hashed (Hash alg)
  deriving stock   ( Show, Read, Generic, Generic1)
  deriving newtype ( Eq,Ord,NFData, Serialise
                   , JSON.FromJSON, JSON.ToJSON, JSON.ToJSONKey, JSON.FromJSONKey
                   , ByteRepr
                   )

instance Show1 (Hashed alg) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (Hashed alg) where
  liftEq _ (Hashed h1) (Hashed h2) = h1 == h2


-- | Algorithm for computing cryptographic hash. We expose fold
--   structure of hash function. Folding is performed inside ST monad
--   which allows to use mutable accumulators for performance.
class (ByteReprSized (Hash alg)) => CryptoHash alg where
  -- | Mutable accumulator for hash function
  data HashAccum alg :: * -> *
  -- | Create new empty hash accumulator
  newHashAccum :: ST s (HashAccum alg s)
  -- | Push chunk of data into accumulator.
  updateHashAccum :: HashAccum alg s -> ByteString -> ST s ()
  -- | Extract digest from accumulator
  freezeHashAccum :: HashAccum alg s -> ST s (Hash alg)


-- | Compute hash of bytestring
hashBlob :: CryptoHash alg => ByteString -> Hash alg
hashBlob bs = runST $ do
  s <- newHashAccum
  updateHashAccum s bs
  freezeHashAccum s

-- | Type class which describes how value should be hashed. We try to
--   take care that different nonprimitive data types do not share
--   representation so it's not possible to get two different values
--   to share hash.
--
--   This type class allows to derive instances using 'Generic'
--   deriving. Deriving uses both name of type and name of
--   constructor.
class CryptoHashable a where
  -- | This function describes how to compute hash of the data
  --   type. For hierarchical records it's computed by default using
  --   depth first traversal.
  hashStep :: CryptoHash alg => HashAccum alg s -> a -> ST s ()
  default hashStep :: (CryptoHash alg, Generic a, GCryptoHashable (Rep a))
                   => HashAccum alg s -> a -> ST s ()
  hashStep = genericHashStep

-- | Analog of 'CryptoHashable' where we may compute hash of type as
--   opposed to hash of value.
class CryptoTypeHashable a where
  hashTypeStep :: CryptoHash alg => HashAccum alg s -> proxy a -> ST s ()


-- | Compute hash of value. It's first serialized using CBOR and then
--   hash of encoded data is computed,
hash :: (CryptoHash alg, CryptoHashable a) => a -> Hash alg
hash a = runST $ do
  s <- newHashAccum
  hashStep s a
  freezeHashAccum s


-- | Size of hash in bytes
hashSize :: forall alg proxy i. (CryptoHash alg, Num i) => proxy alg -> i
hashSize _ = fromIntegral $ natVal (Proxy @(ByteSize (Hash alg)))


hashed :: (CryptoHash alg, CryptoHashable a) => a -> Hashed alg a
hashed = Hashed . hash


----------------------------------------

instance ByteRepr (Hash alg) where
  decodeFromBS         = Just . Hash
  encodeToBS (Hash bs) = bs

instance Show (Hash alg) where
  showsPrec n h
    = showParen (n > 10)
    $ showString "Hash " . shows (encodeBSBase58 $ encodeToBS h)

instance Read (Hash alg) where
  readPrec = do void $ lift $ string "Hash" >> some (char ' ')
                val <- readPrecBSBase58
                case decodeFromBS val of
                  Nothing -> fail "Incorrect bytestring representation of Hash"
                  Just h  -> return h

instance JSON.ToJSON   (Hash alg) where
  toJSON    = defaultToJSON
instance JSON.FromJSON (Hash alg) where
  parseJSON = defaultParseJSON "Hash"
instance JSON.FromJSONKey (Hash alg)
instance JSON.ToJSONKey   (Hash alg)



----------------------------------------------------------------
-- CryptoHashable instances
----------------------------------------------------------------

-- | Data type prefix for defining 'CryptoHashable' instances.
data DataType
  -- Primitives
  = ByteString                   -- ^ Value is a bytestring, i.e. sequence of bytes
  | Text                         -- ^ Value is text
  | PrimI8
  | PrimI16
  | PrimI32
  | PrimI64
  | PrimW8
  | PrimW16
  | PrimW32
  | PrimW64

  -- Structural data types composite types
  | Tuple      !Word16           -- ^ Tuple of size N
  | Sequence   !Word32           -- ^ Sequence of elements of same type with given length
  | MaybeTy
  | SumType    !Word16           -- ^ Tagged sum types with n constructors with 1 field each

  --
  | BaseType   !ByteString       -- ^ Data type defined in standard library
  | CryptoType !ByteString       -- ^ Cryptography related data type
  | UserType   !ByteString       -- ^ User defined type which is identified by name
  deriving (Show)

-- | Constructor identifier for defining 'CryptoHashable' instances.
data Constructor
  = ConstructorIdx  !Int64
  | ConstructorName !ByteString
  deriving (Show)

instance CryptoHashable DataType where
  hashStep s dat = do
    storableHashStep s $ typeID dat
    case dat of
      -- Primitives
      ByteString    -> return ()
      Text          -> return ()
      PrimI8        -> return ()
      PrimI16       -> return ()
      PrimI32       -> return ()
      PrimI64       -> return ()
      PrimW8        -> return ()
      PrimW16       -> return ()
      PrimW32       -> return ()
      PrimW64       -> return ()
      -- Structures
      Tuple    n    -> storableHashStep s n
      Sequence n    -> storableHashStep s n
      MaybeTy       -> return ()
      SumType  n    -> storableHashStep s n
      -- Composites
      BaseType   bs -> hashByteString s bs
      CryptoType bs -> hashByteString s bs
      UserType   bs -> hashByteString s bs
    where
      typeID :: DataType -> Word16
      typeID = \case
        ByteString   -> 0
        Text         -> 1
        PrimI8       -> 2
        PrimI16      -> 3
        PrimI32      -> 4
        PrimI64      -> 5
        PrimW8       -> 6
        PrimW16      -> 7
        PrimW32      -> 8
        PrimW64      -> 9
        -- Structures
        Tuple{}      -> 0x0100 + 0
        Sequence{}   -> 0x0100 + 1
        MaybeTy      -> 0x0100 + 2
        SumType{}    -> 0x0100 + 4
        -- More complicated
        BaseType{}   -> 0x0200 + 0
        CryptoType{} -> 0x0200 + 1
        UserType{}   -> 0x0200 + 2



instance CryptoHashable Constructor where
  hashStep s = \case
    ConstructorIdx  i  -> do storableHashStep s (0 :: Word16)
                             storableHashStep s i
    ConstructorName bs -> do storableHashStep s (1 :: Word16)
                             hashStep         s bs


----------------------------------------
-- Primitives

instance CryptoHashable Int64  where hashStep s i = hashStep s PrimI64 >> storableHashStep s i
instance CryptoHashable Int32  where hashStep s i = hashStep s PrimI32 >> storableHashStep s i
instance CryptoHashable Int16  where hashStep s i = hashStep s PrimI16 >> storableHashStep s i
instance CryptoHashable Int8   where hashStep s i = hashStep s PrimI8  >> storableHashStep s i
instance CryptoHashable Word64 where hashStep s i = hashStep s PrimW64 >> storableHashStep s i
instance CryptoHashable Word32 where hashStep s i = hashStep s PrimW32 >> storableHashStep s i
instance CryptoHashable Word16 where hashStep s i = hashStep s PrimW16 >> storableHashStep s i
instance CryptoHashable Word8  where hashStep s i = hashStep s PrimW8  >> storableHashStep s i

instance CryptoHashable Int where
  hashStep s i = hashStep s (fromIntegral i :: Int64)
instance CryptoHashable Word where
  hashStep s i = hashStep s (fromIntegral i :: Word64)
instance CryptoHashable Char where
  hashStep s = hashStep s . fromEnum
-- FIXME: ZZZ (placeholder)
instance CryptoHashable Integer where
  hashStep s = hashStep s . CBOR.serialise  

instance CryptoHashable ByteString where
  hashStep = updateHashAccum
instance CryptoHashable BL.ByteString where
  hashStep s bs = forM_ (BL.toChunks bs) $ updateHashAccum s
  

----------------------------------------
-- Normal data types

instance CryptoHashable a => CryptoHashable [a] where
  hashStep s xs = do hashStep s $ Sequence $ fromIntegral $ length xs
                     mapM_ (hashStep s) xs

instance CryptoHashable a => CryptoHashable (NE.NonEmpty a) where
  hashStep s xs = do hashStep s $ Sequence $ fromIntegral $ length xs
                     mapM_ (hashStep s) xs

instance (CryptoHashable k, CryptoHashable v) => CryptoHashable (Map.Map k v) where
  -- FIXME: ZZZ
  hashStep s = hashStep s . Map.toList
instance (CryptoHashable a) => CryptoHashable (Set.Set a) where
  -- FIXME: ZZZ
  hashStep s = hashStep s . Set.toList

instance CryptoHashable a => CryptoHashable (Maybe a) where
  hashStep s m = do
    hashStep s MaybeTy
    case m of Just x  -> do hashStep s $ ConstructorIdx 0
                            hashStep s x
              Nothing -> do hashStep s $ ConstructorIdx 1

instance (CryptoHashable a, CryptoHashable b) => CryptoHashable (Either a b) where
  hashStep s m = do
    hashStep s $ SumType 2
    case m of Left  a -> do hashStep s $ ConstructorIdx 0
                            hashStep s a
              Right b -> do hashStep s $ ConstructorIdx 1
                            hashStep s b

instance CryptoHashable () where
  hashStep s () = hashStep s $ Tuple 0

instance (CryptoHashable a, CryptoHashable b) => CryptoHashable (a, b) where
  hashStep s (a,b) = do
    hashStep s $ Tuple 2
    hashStep s a
    hashStep s b

instance (CryptoHashable a, CryptoHashable b, CryptoHashable c) => CryptoHashable (a, b, c) where
  hashStep s (a,b,c) = do
    hashStep s $ Tuple 3
    hashStep s a
    hashStep s b
    hashStep s c

-- FIXME: ZZZ (placeholder instances)
instance CryptoHashable (Hashed alg a) where
  hashStep s (Hashed h) = hashStep s h
instance CryptoHashable (Hash alg) where
  hashStep s = updateHashAccum s . encodeToBS



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

storableHashStep :: (CryptoHash alg, Storable a) => HashAccum alg s -> a -> ST s ()
{-# INLINE storableHashStep #-}
storableHashStep s i
  = updateHashAccum s
  $ unsafePerformIO
  $ BI.create (sizeOf i) (\p -> poke (castPtr p) i)

hashByteString :: (CryptoHash alg) => HashAccum alg s -> ByteString -> ST s ()
hashByteString s bs = do storableHashStep s (fromIntegral $ BS.length bs :: Word32)
                         updateHashAccum  s bs


----------------------------------------------------------------
-- Generics for CryptoHashable
----------------------------------------------------------------

class GCryptoHashable f where
  ghashStep :: CryptoHash alg => HashAccum alg s -> f a -> ST s ()

instance (Datatype d, GCryptoHashable f) => GCryptoHashable (M1 D d f) where
  ghashStep s x@(M1 f) = do
    hashStep  s $ UserType $ BC8.pack $ datatypeName x
    ghashStep s f

instance (GHC.Constructor c, GCryptoHashable f) => GCryptoHashable (M1 C c f) where
  ghashStep s x@(M1 f) = do
    hashStep  s $ ConstructorName $ BC8.pack $ conName x
    ghashStep s f

instance (GCryptoHashable f) => GCryptoHashable (M1 S s f) where
  ghashStep s (M1 f) = ghashStep s f

instance (GCryptoHashable f, GCryptoHashable g) => GCryptoHashable (f :+: g) where
  ghashStep s (L1 f) = ghashStep s f
  ghashStep s (R1 g) = ghashStep s g

instance (GCryptoHashable f, GCryptoHashable g) => GCryptoHashable (f :*: g) where
  ghashStep s (f :*: g) = do ghashStep s f
                             ghashStep s g

instance GCryptoHashable U1 where
  ghashStep _ _ = return ()

instance CryptoHashable a => GCryptoHashable (K1 i a) where
  ghashStep s (K1 a) = hashStep s a

genericHashStep
  :: (Generic a, GCryptoHashable (Rep a), CryptoHash alg)
  => HashAccum alg s -> a -> ST s ()
genericHashStep s a = ghashStep s (from a)
