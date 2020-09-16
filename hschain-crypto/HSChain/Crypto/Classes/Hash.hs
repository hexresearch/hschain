{-# LANGUAGE DataKinds                  #-}
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
{-# LANGUAGE UndecidableInstances       #-}
-- |
module HSChain.Crypto.Classes.Hash (
    -- * Data types and API
    Hash(..)
  , Hashed(..)
  , CryptoHash(..)
  , hashSize
  , CryptoHashable(..)
  , CryptoTypeHashable(..)
  , hash
  , hashed
    -- * Hash API
  , CryptoName(..)
  , DataType(..)
  , Constructor(..)
    -- ** Deriving via
  , CryptoHashablePackage(..)
    -- ** Generics derivation
  , GCryptoHashable(..)
  , genericHashStep
  , genericHashStepTy
  ) where

import Codec.Serialise   (Serialise)
import Control.Applicative
import Control.Monad
import Control.DeepSeq

import qualified Data.Aeson               as JSON
import           Data.ByteString            (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BC8
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString.Builder       as Bld
import qualified Data.ByteString.Builder.Extra as Bld
import qualified Data.Map.Strict          as Map
import qualified Data.Sequence            as Seq
import qualified Data.Set                 as Set
import qualified Data.List.NonEmpty       as NE
import qualified Data.Vector              as VecV
import qualified Data.Vector.Unboxed      as VecU
import qualified Data.Vector.Storable     as VecS
import qualified Data.Vector.Primitive    as VecP
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import Data.Bits
import Data.Functor.Classes
import Data.String
import Data.Word
import Data.Proxy
import Data.Int
import Numeric.Natural
import Text.Read
import Text.ParserCombinators.ReadP

import GHC.TypeLits
import GHC.Generics hiding (Constructor)
import qualified GHC.Generics as GHC

import HSChain.Crypto.Classes


----------------------------------------------------------------
-- Cryptographic hashes
----------------------------------------------------------------

-- | Cryptographic hash of sequence of bytes.
newtype Hash alg = Hash BS.ByteString
  deriving stock   (Generic, Generic1)
  deriving newtype (Eq,Ord,Serialise,NFData)

-- | Newtype wrapper of 'Hash' that carries type tag for keeping type
--   of value that was hashed.
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


-- | Name of cryptographic algorithm. It's used in implementation of
--   'CryptoHashable' instances of 'Hash'
newtype CryptoName alg = CryptoName { getCryptoName :: ByteString }
  deriving stock   (Show)
  deriving newtype (IsString)

-- | Algorithm for computing cryptographic hash. In the name of
--   simlicity we do not expose fold structure of hash function. In
--   practice lazy bytestrings turned out to be good enough
--   substitute.
class (ByteReprSized (Hash alg)) => CryptoHash alg where
  -- | Compute hash of strict bytestring
  hashBlob     :: BS.ByteString -> Hash alg
  -- | Compute hash of lazy bytestring
  hashLazyBlob :: BL.ByteString -> Hash alg
  -- | Hash output of a builder
  hashBuilder :: Bld.Builder -> Hash alg
  hashBuilder = hashLazyBlob
              . Bld.toLazyByteStringWith (Bld.untrimmedStrategy 4096 4096) mempty
  -- | Name of algorithm
  hashAlgorithmName :: CryptoName alg

-- | Type class which describes how value should be hashed. Goal of
--   this type class is to separate hashing from serialization, allow
--   more complicated hashing schemes (Merkle trees for example). In a
--   sense this type class describes how to serialize value without
--   way to deserialize it back.
--
--   API is typeclass based and tries satisfy conflicting
--   requirements. First we want our encoding to be compact: we want
--   to hash as little data as possible. Then for each type we have to
--   choose between nominal and structural encoding. First is to ensure
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
  hashStep :: a -> Bld.Builder

-- | Analog of 'CryptoHashable' where we may compute hash of type as
--   opposed to hash of value.
class CryptoTypeHashable a where
  hashTypeStep :: proxy a -> Bld.Builder


-- | Compute hash of haskell value. Mapping of value to bytes which
--   are consequently hashed is described by 'CryptoHashable' type class.
hash :: (CryptoHash alg, CryptoHashable a) => a -> Hash alg
{-# INLINABLE hash #-}
hash = hashLazyBlob
     . Bld.toLazyByteStringWith (Bld.untrimmedStrategy 4096 4096) mempty
     . hashStep

-- | Size of hash in bytes
hashSize :: forall alg proxy i. (CryptoHash alg, Num i) => proxy alg -> i
hashSize _ = fromIntegral $ natVal (Proxy @(ByteSize (Hash alg)))

-- | Compute hash of value. Works same as 'hash' but keeps type of
--   value as phantom parameter.
hashed :: (CryptoHash alg, CryptoHashable a) => a -> Hashed alg a
hashed = Hashed . hash


-- | Newtype wrapper for deriving CryptoHashable using DerivingVia.
newtype CryptoHashablePackage (str :: Symbol) a = CryptoHashablePackage a

instance ( Generic a
         , GCryptoHashable (Rep a)
         , KnownSymbol str
         ) => CryptoHashable (CryptoHashablePackage str a) where
  hashStep (CryptoHashablePackage a) =
    ghashStepPkg (symbolVal (Proxy @str)) (from a)

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
  | PrimTruth                   -- ^ Truth value
  | PrimFalse                   -- ^ False value

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
  -- ^ Public key for a given algorithm
  | CryPrivateKey  !ByteString
  -- ^ Private key for a given algorithm
  | CrySignature   !ByteString
  -- ^ Signature which uses given scheme
  deriving (Show)

-- | Constructor identifier for defining 'CryptoHashable' instances.
data Constructor
  = ConstructorIdx  !Word16
  | ConstructorName !ByteString
  deriving (Show)


instance CryptoHashable DataType where
  hashStep dat
    = Bld.word16LE (dataTypeTag dat)
   <> case dat of
        -- Primitives
        PrimBytes n   -> Bld.word32LE n
        PrimI8        -> mempty
        PrimI16       -> mempty
        PrimI32       -> mempty
        PrimI64       -> mempty
        PrimW8        -> mempty
        PrimW16       -> mempty
        PrimW32       -> mempty
        PrimW64       -> mempty
        PrimChar      -> mempty
        PrimTruth     -> mempty
        PrimFalse     -> mempty
        -- Structures
        TyTuple    n    -> Bld.word16LE n
        TySequence n    -> Bld.word32LE n
        TyMap      n    -> Bld.word32LE n
        TySet      n    -> Bld.word32LE n
        TyNothing       -> mempty
        TyJust          -> mempty
        TyBase     bs   -> hashStep bs
        -- Composites
        UserType m n    -> nullTerminatedString m
                        <> nullTerminatedString n
        -- Crypto primitives
        CryHash        bs    -> hashStep bs
        CryFingerprint bH bK -> hashStep bH <> hashStep bK
        CryPublicKey   bs    -> hashStep bs
        CryPrivateKey  bs    -> hashStep bs
        CrySignature   bs    -> hashStep bs

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
  PrimTruth    -> 10
  PrimFalse    -> 11
  -- Structures
  TyTuple{}    -> 0x0100 + 0
  TySequence{} -> 0x0100 + 1
  TyMap{}      -> 0x0100 + 2
  TySet{}      -> 0x0100 + 3
  TyNothing    -> 0x0100 + 4
  TyJust       -> 0x0100 + 5
  TyBase{}     -> 0x0100 + 6
  -- User defined data
  UserType{}   -> 0x0200
  -- Crypto primitives
  CryHash{}        -> 0x0300 + 0
  CryFingerprint{} -> 0x0300 + 1
  CryPublicKey{}   -> 0x0300 + 2
  CryPrivateKey{}  -> 0x0300 + 3
  CrySignature{}   -> 0x0300 + 4

instance CryptoHashable Constructor where
  hashStep = \case
    ConstructorIdx  i  -> Bld.word16LE 0
                       <> Bld.word16LE i
    ConstructorName bs -> Bld.word16LE 1
                       <> hashStep bs


----------------------------------------
-- Primitives

instance CryptoHashable Int64  where hashStep i = hashStep PrimI64 <> Bld.int64LE  i
instance CryptoHashable Int32  where hashStep i = hashStep PrimI32 <> Bld.int32LE  i
instance CryptoHashable Int16  where hashStep i = hashStep PrimI16 <> Bld.int16LE  i
instance CryptoHashable Int8   where hashStep i = hashStep PrimI8  <> Bld.int8     i
instance CryptoHashable Word64 where hashStep i = hashStep PrimW64 <> Bld.word64LE i
instance CryptoHashable Word32 where hashStep i = hashStep PrimW32 <> Bld.word32LE i
instance CryptoHashable Word16 where hashStep i = hashStep PrimW16 <> Bld.word16LE i
instance CryptoHashable Word8  where hashStep i = hashStep PrimW8  <> Bld.word8    i

instance CryptoHashable Bool where
  hashStep True  = hashStep PrimTruth
  hashStep False = hashStep PrimFalse

-- | Same as 'Int64'
instance CryptoHashable Int where
  hashStep i = hashStep (fromIntegral i :: Int64)
-- | Same as 'Word64'
instance CryptoHashable Word where
  hashStep i = hashStep (fromIntegral i :: Word64)

-- | 32-bit unicode code point
instance CryptoHashable Char where
  hashStep c = hashStep PrimChar
            <> Bld.word32LE (fromIntegral (fromEnum c))

instance CryptoHashable Integer where
  hashStep = hashIntegerStep
instance CryptoHashable Natural where
  hashStep = hashIntegerStep


-- Integer is encoded in unsigned 63-bit chunks. Highest bit is
-- continuation bit which is set to 1 if there're more chunks and 0
-- otherwise. First chunk is only 62-bit since bit 62 is used to
-- store sign.
--
-- This is to ensure that value of different type couldn't hash to
-- same value.  Without continuation bit it's impossible to know how
-- many chunks we have and it's difficult to compute their number in
-- advance
--
-- We encode first chunk 30 bit of number
--  Bit 63 - 1 if there're more chunks
--  Bit 62 - sign
hashIntegerStep :: (Bits a, Integral a) => a -> Bld.Builder
hashIntegerStep n
  = (Bld.word64LE  $ writeBit 63 (next /= 0)
                   $ writeBit 62 (n < 0)
                   $ fromIntegral n')
 <> if next /= 0 then integerStep next else mempty
  where
    n'   = abs n
    next = n' `shiftR` 62

-- Then we encode every subsequent 31 bit.
--
--  Bit 31 - 1 if there're more chunks
integerStep :: (Bits a, Integral a) => a -> Bld.Builder
integerStep n
  = (Bld.word64LE $ writeBit 63 (next /= 0)
                  $ fromIntegral n)
 <> if next /= 0 then integerStep next else mempty
  where
    next = n `shiftR` 63
--
writeBit :: Bits a => Int -> Bool -> a -> a
writeBit i True  x = setBit   x i
writeBit i False x = clearBit x i


instance CryptoHashable ByteString where
  hashStep bs = hashStep (PrimBytes $ fromIntegral $ BS.length bs)
             <> Bld.byteString bs

instance CryptoHashable BL.ByteString where
  hashStep bs = hashStep (PrimBytes $ fromIntegral $ BL.length bs)
             <> Bld.lazyByteString bs

instance CryptoHashable T.Text where
  hashStep ts = hashStep (TySequence $ fromIntegral $ T.length ts)
             <> T.foldr  (\c b -> hashStep c <> b) mempty ts

instance CryptoHashable TL.Text where
  hashStep ts = hashStep (TySequence $ fromIntegral $ TL.length ts)
             <> TL.foldr  (\c b -> hashStep c <> b) mempty ts

----------------------------------------
-- Normal data types

instance CryptoHashable a => CryptoHashable [a] where
  hashStep xs = hashStep (TySequence $ fromIntegral $ length xs)
             <> foldMap hashStep xs
  {-# INLINABLE hashStep #-}

instance CryptoHashable a => CryptoHashable (Seq.Seq a) where
  hashStep xs = hashStep (TySequence $ fromIntegral $ Seq.length xs)
             <> foldMap hashStep xs
  {-# INLINABLE hashStep #-}

instance CryptoHashable a => CryptoHashable (NE.NonEmpty a) where
  hashStep xs = hashStep (TySequence $ fromIntegral $ length xs)
             <> foldMap hashStep xs
  {-# INLINABLE hashStep #-}

instance (CryptoHashable k, CryptoHashable v) => CryptoHashable (Map.Map k v) where
  hashStep xs = hashStep (TyMap $ fromIntegral $ length xs)
             <> foldMap hashStep (Map.toAscList xs)
  {-# INLINABLE hashStep #-}

instance (CryptoHashable a) => CryptoHashable (Set.Set a) where
  hashStep xs = hashStep (TySet $ fromIntegral $ length xs)
             <> foldMap hashStep (Set.toAscList xs)
  {-# INLINABLE hashStep #-}

instance (CryptoHashable a) => CryptoHashable (VecV.Vector a) where
  hashStep xs = hashStep (TySequence $ fromIntegral $ VecV.length xs)
             <> VecV.foldl (\b x -> b <> hashStep x) mempty xs
  {-# INLINE hashStep #-}
instance (CryptoHashable a, VecP.Prim a) => CryptoHashable (VecP.Vector a) where
  hashStep xs = hashStep (TySequence $ fromIntegral $ VecP.length xs)
             <> VecP.foldl (\b x -> b <> hashStep x) mempty xs
  {-# INLINE hashStep #-}
instance (CryptoHashable a, VecU.Unbox a) => CryptoHashable (VecU.Vector a) where
  hashStep xs = hashStep (TySequence $ fromIntegral $ VecU.length xs)
             <> VecU.foldl (\b x -> b <> hashStep x) mempty xs
  {-# INLINE hashStep #-}
instance (CryptoHashable a, VecS.Storable a) => CryptoHashable (VecS.Vector a) where
  hashStep xs = hashStep (TySequence $ fromIntegral $ VecS.length xs)
             <> VecS.foldl (\b x -> b <> hashStep x) mempty xs
  {-# INLINE hashStep #-}

instance CryptoHashable a => CryptoHashable (Maybe a) where
  hashStep Nothing  = hashStep TyNothing
  hashStep (Just a) = hashStep TyJust
                   <> hashStep a
  {-# INLINABLE hashStep #-}

instance (CryptoHashable a, CryptoHashable b) => CryptoHashable (Either a b) where
  hashStep m = hashStep (TyBase "Either")
            <> case m of Left  a -> hashStep (ConstructorIdx 0)
                                 <> hashStep a
                         Right b -> hashStep (ConstructorIdx 1)
                                 <> hashStep b
  {-# INLINABLE hashStep #-}

instance CryptoHashable () where
  hashStep () = hashStep $ TyTuple 0

instance (CryptoHashable a, CryptoHashable b) => CryptoHashable (a, b) where
  hashStep (a,b) = hashStep (TyTuple 2)
                <> hashStep a
                <> hashStep b
  {-# INLINABLE hashStep #-}

instance (CryptoHashable a, CryptoHashable b, CryptoHashable c) => CryptoHashable (a, b, c) where
  hashStep (a,b,c) = hashStep (TyTuple 3)
                  <> hashStep a
                  <> hashStep b
                  <> hashStep c
  {-# INLINABLE hashStep #-}

instance CryptoHash alg => CryptoHashable (Hashed alg a) where
  hashStep (Hashed h) = hashStep h
  {-# INLINABLE hashStep #-}

instance CryptoHash alg => CryptoHashable (Hash alg) where
  hashStep (Hash bs) = hashStep (CryHash $ getCryptoName (hashAlgorithmName @alg))
                    <> hashStep bs
  {-# INLINABLE hashStep #-}


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Null terminated string
nullTerminatedString :: [Char] -> Bld.Builder
nullTerminatedString xs = Bld.stringUtf8 xs <> Bld.word32LE 0


----------------------------------------------------------------
-- Generics for CryptoHashable
----------------------------------------------------------------

-- | Generic deriving for user-defined data types. All methods here
--   require to pass library name manually. While generics provide
--   package anem it's overly specific and includes for example
--   verion. Thus it would be better to require package name from
--   user.
class GCryptoHashable f where
  -- | Default implementation of 'CryptoHashable'.
  ghashStepPkg
    :: String          -- ^ Library name
    -> f a
    -> Bld.Builder
  -- | Same as 'genericHashStep' but allows to override type name as
  --   well.
  ghashStepPkgTy
    :: String          -- ^ Library name
    -> String          -- ^ Data type name
    -> f a
    -> Bld.Builder

-- | Generic implementation of 'CryptoHashable'
genericHashStep
  :: (Generic a, GCryptoHashable (Rep a))
  => String           -- ^ Library name
  -> a
  -> Bld.Builder
genericHashStep pkg = ghashStepPkg pkg . from
{-# INLINE genericHashStep #-}

-- | Generic implementation of 'CryptoHashable' which allows to
--   override data type name as well
genericHashStepTy
  :: (Generic a, GCryptoHashable (Rep a))
  => String           -- ^ Library name
  -> String           -- ^ Data type name
  -> a
  -> Bld.Builder
genericHashStepTy pkg ty = ghashStepPkgTy pkg ty . from
{-# INLINE genericHashStepTy #-}



instance (Datatype d, GCryptoHashableWorker f) => GCryptoHashable (M1 D d f) where
  ghashStepPkg pkg x@(M1 f)
    = hashStep (UserType pkg $ datatypeName x)
   <> ghashStep f
  ghashStepPkgTy pkg con (M1 f)
    = hashStep (UserType pkg con)
   <> ghashStep f


class GCryptoHashableWorker f where
  ghashStep :: f a -> Bld.Builder

instance (GHC.Constructor c, GCryptoHashableWorker f) => GCryptoHashableWorker (M1 C c f) where
  ghashStep x@(M1 f)
    = hashStep (ConstructorName $ BC8.pack $ conName x)
   <> ghashStep f

instance (GCryptoHashableWorker f) => GCryptoHashableWorker (M1 S s f) where
  ghashStep (M1 f) = ghashStep f

instance (GCryptoHashableWorker f, GCryptoHashableWorker g) => GCryptoHashableWorker (f :+: g) where
  ghashStep (L1 f) = ghashStep f
  ghashStep (R1 g) = ghashStep g

instance (GCryptoHashableWorker f, GCryptoHashableWorker g) => GCryptoHashableWorker (f :*: g) where
  ghashStep (f :*: g) = ghashStep f <> ghashStep g

instance GCryptoHashableWorker U1 where
  ghashStep _ = mempty

instance CryptoHashable a => GCryptoHashableWorker (K1 i a) where
  ghashStep (K1 a) = hashStep a
