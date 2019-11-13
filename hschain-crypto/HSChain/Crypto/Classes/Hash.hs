{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-- |
module HSChain.Crypto.Classes.Hash (
    -- * Data types and API
    Hash(..)
  , Hashed(..)
  , CryptoHash(..)
  , hashBlob
  , hashSize
  , CryptoHashable(..)
  , CryptoTypeHashable(..)
  , hash
  , hashed
    -- * Hash API
    -- $hash_encoding
  , CryptoName(..)
  , DataType(..)
  , Constructor(..)
    -- ** Helpers
  , storableHashStep
    -- ** Generics derivation
  , GCryptoHashable(..)
  ) where

import Codec.Serialise   (Serialise)
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
import Data.Bits
import Data.Functor.Classes
import Data.String
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


-- | Name of cryptographic algorithm
newtype CryptoName alg = CryptoName { getCryptoName :: ByteString }
  deriving stock   (Show)
  deriving newtype (IsString)

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
  -- | Name of algorithm
  hashAlgorithmName :: CryptoName alg

-- | Compute hash of bytestring
hashBlob :: CryptoHash alg => ByteString -> Hash alg
hashBlob bs = runST $ do
  s <- newHashAccum
  updateHashAccum s bs
  freezeHashAccum s

-- | Type class which describes how value should be hashed. Goal of
--   this type class is to separate hashing from serialization, allow
--   more complicated hashing schemes (Merkle trees for example). In a
--   sense this type class describes how to serialize value without
--   way to deserialize it back.
--
--   API is typeclass based and tries satisfy conflicting
--   requirements. First we want our encoding to be compact: we want
--   to hash as little data as possible. Then for each type we have to
--   chose between nominal and structural encoding. First is to ensure
--   that two values of different types never hash to same value
--   excluding possibility of hash collision. Second is to treat value
--   as some generic structure for example list, vector should have to
--   same value.
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


-- | Compute hash of value using 'CryptoHashable'
hash :: (CryptoHash alg, CryptoHashable a) => a -> Hash alg
hash a = runST $ do
  s <- newHashAccum
  hashStep s a
  freezeHashAccum s


-- | Size of hash in bytes
hashSize :: forall alg proxy i. (CryptoHash alg, Num i) => proxy alg -> i
hashSize _ = fromIntegral $ natVal (Proxy @(ByteSize (Hash alg)))

-- | Compute hash of value. Works same as 'hash' but keeps type of
--   value as phantom parameter.
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

-- $hash_encoding
--
-- Encoding uses following structure.
--
--  1. Each data type is prefixed with type tag. In haskell code it's
--     represented by 'DataType'
--



-- | Data type tag for instances of 'CryptoHashable'. Its instances
--   just writes bare type tag. It
data DataType
  -- Primitives
  = PrimBytes !Word32
  -- ^ Value is a bytestring, i.e. sequence of bytes. Field is length
  --   of bytestring
  | PrimI8                      -- ^ Signed 8-bit integer
  | PrimI16                     -- ^ Signed 16-bit integer
  | PrimI32                     -- ^ Signed 32-bit integer
  | PrimI64                     -- ^ Signed 64-bit integer
  | PrimW8                      -- ^ Unsigned 8-bit integer
  | PrimW16                     -- ^ Unsigned 16-bit integer
  | PrimW32                     -- ^ Unsigned 32-bit integer
  | PrimW64                     -- ^ Unsigned 64-bit integer
  | PrimChar                    -- ^ Char

  -- Structural data types composite types
  | TyTuple    !Word16
  -- ^ Tuple of size N
  | TySequence !Word32
  -- ^ Sequence of elements of same type with given length
  | TyMap      !Word32
  -- ^ Map. It's expected to be represented as sequence of pair. So
  --   it's not very different from TySequence but elements need to
  --   appear sorted by key
  | TySet      !Word32
  -- ^ Set of elements. Represented in the same way as TySequence but
  --   elements should be sorted
  | TyNothing
  -- ^ Absense of value.
  | TyJust
  -- ^ Just a single value
  | TyBase   !ByteString
  -- ^ Type defined in starndard (base) or other libraries considered
  --   standard

  | UserType String String
  -- ^ User defined type. In this case it's important to avoid
  --   accidental clashes between different data types. Especially
  --   when they are defined in different libaries. As such user type
  --   is described by libary name and data type name.

  | CryHash !ByteString
  -- ^ Hash. It first write algorithm name, then hash itself
  | CryFingerprint !ByteString !ByteString
  -- ^ Fingerprint of public key. It first writes hash algorithm name,
  --   then nake of algorithms for asymmetric cryptography
  | CryPublicKey   !ByteString
  | CryPrivateKey  !ByteString
  | CrySignature   !ByteString
  deriving (Show)

-- | Constructor identifier for defining 'CryptoHashable' instances.
data Constructor
  = ConstructorIdx  !Int64
  | ConstructorName !ByteString
  deriving (Show)


instance CryptoHashable DataType where
  hashStep s dat = do
    storableHashStep s $ dataTypeTag dat
    case dat of
      -- Primitives
      PrimBytes n   -> storableHashStep s n
      PrimI8        -> return ()
      PrimI16       -> return ()
      PrimI32       -> return ()
      PrimI64       -> return ()
      PrimW8        -> return ()
      PrimW16       -> return ()
      PrimW32       -> return ()
      PrimW64       -> return ()
      PrimChar      -> return ()
      -- Structures
      TyTuple    n    -> storableHashStep s n
      TySequence n    -> storableHashStep s n
      TyMap      n    -> storableHashStep s n
      TySet      n    -> storableHashStep s n
      TyNothing       -> return ()
      TyJust          -> return ()
      TyBase     bs   -> hashStep s bs
      -- Composites
      UserType m n    -> do nullTerminatedString s m
                            nullTerminatedString s n
      -- Crypto primitives
      CryHash        bs    -> hashStep s bs
      CryFingerprint bH bK -> hashStep s bH >> hashStep s bK
      CryPublicKey   bs    -> hashStep s bs
      CryPrivateKey  bs    -> hashStep s bs
      CrySignature   bs    -> hashStep s bs

dataTypeTag :: DataType -> Word16
dataTypeTag = \case
  PrimBytes{}  -> 0
  PrimI8       -> 1
  PrimI16      -> 2
  PrimI32      -> 3
  PrimI64      -> 4
  PrimW8       -> 5
  PrimW16      -> 6
  PrimW32      -> 7
  PrimW64      -> 8
  PrimChar     -> 9
  -- Structures
  TyTuple{}    -> 0x0100 + 0
  TySequence{} -> 0x0100 + 1
  TyMap{}      -> 0x0100 + 2
  TySet{}      -> 0x0100 + 3
  TyNothing    -> 0x0100 + 4
  TyJust       -> 0x0100 + 5
  TyBase{}     -> 0x0100 + 6
  --
  UserType{}   -> 0x0200
  --
  CryHash{}        -> 0x0300 + 0
  CryFingerprint{} -> 0x0300 + 1
  CryPublicKey{}   -> 0x0300 + 2
  CryPrivateKey{}  -> 0x0300 + 3
  CrySignature{}   -> 0x0300 + 4

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
  hashStep s c = do
    hashStep s PrimChar
    hashStep s (fromIntegral (fromEnum c) :: Word32)

instance CryptoHashable Integer where
  hashStep s = start
    where
      -- We encode first chunk 30 bit of number
      --  Bit 31 - 1 if there're more chunks
      --  Bit 30 - sign
      start n = do
        storableHashStep s $ writeBit 31 (next /= 0)
                           $ writeBit 30 (n < 0)
                           $ (fromIntegral n' :: Word32)
        when (next /= 0) $ istep next
        where
          n'   = abs n
          next = n' `shiftR` 30
      -- The we encode every subsequent 31 bit.
      --
      --  Bit 31 - 1 if there're more chunks
      istep n = do
        storableHashStep s $ writeBit 31 (next /= 0)
                           $ (fromIntegral n :: Word32)
        when (next /= 0) $ istep next
        where
          next = n `shiftR` 31
      --
      writeBit :: Bits a => Int -> Bool -> a -> a
      writeBit i True  x = setBit   x i
      writeBit i False x = clearBit x i


instance CryptoHashable ByteString where
  hashStep s bs = do hashStep s $ PrimBytes $ fromIntegral $ BS.length bs
                     updateHashAccum s bs
instance CryptoHashable BL.ByteString where
  hashStep s bs = do hashStep s $ PrimBytes $ fromIntegral $ BL.length bs
                     forM_ (BL.toChunks bs) $ updateHashAccum s


----------------------------------------
-- Normal data types

instance CryptoHashable a => CryptoHashable [a] where
  hashStep s xs = do hashStep s $ TySequence $ fromIntegral $ length xs
                     mapM_ (hashStep s) xs

instance CryptoHashable a => CryptoHashable (NE.NonEmpty a) where
  hashStep s xs = do hashStep s $ TySequence $ fromIntegral $ length xs
                     mapM_ (hashStep s) xs

instance (CryptoHashable k, CryptoHashable v) => CryptoHashable (Map.Map k v) where
  hashStep s xs = do hashStep s $ TyMap $ fromIntegral $ length xs
                     mapM_ (hashStep s) $ Map.toList xs

instance (CryptoHashable a) => CryptoHashable (Set.Set a) where
  hashStep s xs = do hashStep s $ TySet $ fromIntegral $ length xs
                     mapM_ (hashStep s) $ Set.toList xs

instance CryptoHashable a => CryptoHashable (Maybe a) where
  hashStep s Nothing  = do hashStep s TyNothing
  hashStep s (Just a) = do hashStep s TyJust
                           hashStep s a

instance (CryptoHashable a, CryptoHashable b) => CryptoHashable (Either a b) where
  hashStep s m = do
    hashStep s $ TyBase "Either"
    case m of Left  a -> do hashStep s $ ConstructorIdx 0
                            hashStep s a
              Right b -> do hashStep s $ ConstructorIdx 1
                            hashStep s b

instance CryptoHashable () where
  hashStep s () = hashStep s $ TyTuple 0

instance (CryptoHashable a, CryptoHashable b) => CryptoHashable (a, b) where
  hashStep s (a,b) = do
    hashStep s $ TyTuple 2
    hashStep s a
    hashStep s b

instance (CryptoHashable a, CryptoHashable b, CryptoHashable c) => CryptoHashable (a, b, c) where
  hashStep s (a,b,c) = do
    hashStep s $ TyTuple 3
    hashStep s a
    hashStep s b
    hashStep s c


instance CryptoHash alg => CryptoHashable (Hashed alg a) where
  hashStep s (Hashed h) = hashStep s h

instance CryptoHash alg => CryptoHashable (Hash alg) where
  hashStep s (Hash bs) = do
    hashStep s $ CryHash $ getCryptoName (hashAlgorithmName @alg)
    hashStep s bs


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Hash value using its 'Storable' instance
storableHashStep :: (CryptoHash alg, Storable a) => HashAccum alg s -> a -> ST s ()
{-# INLINE storableHashStep #-}
storableHashStep s i
  = updateHashAccum s
  $ unsafePerformIO
  $ BI.create (sizeOf i) (\p -> poke (castPtr p) i)

-- | Null terminated string
nullTerminatedString :: (CryptoHash alg) => HashAccum alg s -> [Char] -> ST s ()
nullTerminatedString s xs = do
  forM_ xs (hashStep s)
  hashStep s (toEnum 0 :: Char)

----------------------------------------------------------------
-- Generics for CryptoHashable
----------------------------------------------------------------

class GCryptoHashable f where
  ghashStep :: CryptoHash alg => HashAccum alg s -> f a -> ST s ()

instance (Datatype d, GCryptoHashable f) => GCryptoHashable (M1 D d f) where
  ghashStep s x@(M1 f) = do
    -- FIXME: nodule name
    hashStep  s $ UserType "" $ datatypeName x
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
