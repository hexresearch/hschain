{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Simple API for cryptographic operations. Crypto algorithms are
-- selected by type and all necessary types are implemented as data
-- families
module Thundermint.Crypto (
    -- * Crypto API
    PrivKey
  , PublicKey
  , Signature(..)
  , Fingerprint(..)
  , Hash(..)
  , Crypto
  , CryptoSign(..)
  , CryptoHash(..)
  , CryptoSignPrim(..)
  , (:&)
    -- ** Sizes of crypto types
  , hashSize
  , fingerprintSize
  , publicKeySize
  , privKeySize
  , signatureSize
    -- * Petrification
  , Pet
  , pet
  , petrify
    -- * Hashes
  , Hashed(..)
  , hashed
  , hash
    -- * Encoding and decoding of values
  , ByteRepr(..)
  , encodeBase58
  , decodeBase58
  , encodeBSBase58
  , decodeBSBase58
  , readPrecBSBase58
    -- * Serialization and signatures
  , SignedState(..)
  , Signed
  , makeSigned
  , signedValue
  , signedPetrified
  , signedAddr
  , signValue
  , verifySignature
  , unverifySignature
  , signPetrified
  , verifyPetrifiedSignature
  ) where

import Codec.Serialise (Serialise)
import qualified Codec.Serialise          as CBOR
import Control.Applicative
import Control.DeepSeq
import Control.Monad

import qualified Data.Aeson           as JSON
import           Data.Text              (Text)
import           Data.Coerce
import           Data.SafeCopy
import qualified Data.Text.Encoding            as T
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base58        as Base58
import           Data.ByteString.Lazy    (toStrict,fromStrict)
import qualified Data.ByteString.Char8         as BC8
import Data.Char     (isAscii)
import Data.Typeable (Proxy(..))
import Text.Read     (Read(..), ReadPrec,lift)
import Text.ParserCombinators.ReadP
import GHC.TypeNats
import GHC.Generics         (Generic,Generic1)


----------------------------------------------------------------
-- Basic crypto API
----------------------------------------------------------------

type Crypto (alg) = (CryptoSign alg, CryptoHash alg)

data sign :& hash

-- | Private key
data family PrivKey   alg

-- | Public key
data family PublicKey alg

-- | Signature
newtype Signature alg = Signature BS.ByteString
  deriving (Eq, Ord, Generic, Generic1, Serialise, NFData)

-- | Public key fingerprint (hash of public key)
newtype Fingerprint alg = Fingerprint BS.ByteString
  deriving (Eq,Ord, Generic, Generic1, Serialise, NFData)

-- | Cryptographic hash of some value
newtype Hash alg = Hash BS.ByteString
  deriving (Eq,Ord, Generic, Generic1, Serialise, NFData)

class ( ByteRepr (Fingerprint alg)
      , ByteRepr (Signature   alg)
      , ByteRepr (PublicKey   alg)
      , ByteRepr (PrivKey     alg)
      , CryptoSignPrim alg
      ) => CryptoSign alg where

  -- | Sign sequence of bytes
  signBlob            :: PrivKey   alg -> BS.ByteString -> Signature alg
  -- | Check that signature is correct
  verifyBlobSignature :: PublicKey alg -> BS.ByteString -> Signature alg -> Bool
  -- | Compute public key from  private key
  publicKey           :: PrivKey   alg -> PublicKey alg
  -- | Compute fingerprint or public key fingerprint
  fingerprint         :: PublicKey alg -> Fingerprint alg

class ( KnownNat (SignatureSize alg)
      , KnownNat (FingerprintSize alg)
      , KnownNat (PublicKeySize alg)
      , KnownNat (PrivKeySize alg)
      --, CryptoSign alg
      ) => CryptoSignPrim alg where
  type FingerprintSize alg :: Nat
  type PublicKeySize   alg :: Nat
  type PrivKeySize     alg :: Nat
  type SignatureSize   alg :: Nat

-- | Type-indexed set of crypto algorithms. It's not very principled
--   to push everything into singe type class.  But in order to keep
--   signatures sane it was done this way.
class (
--    , CryptoSignPrim alg
       ByteRepr (Hash   alg)
      , KnownNat (HashSize alg)
      ) => CryptoHash alg where
  -- | Compute hash of sequence of bytes
  hashBlob     :: BS.ByteString -> Hash alg

  -- | Compare hash with a bytestring safly
  hashEquality :: Hash alg -> BS.ByteString -> Bool

  type HashSize        alg :: Nat

-- | Size of hash in bytes
hashSize :: forall alg proxy i. (Crypto alg, Num i) => proxy alg -> i
hashSize _ = fromIntegral $ natVal (Proxy :: Proxy (HashSize alg))

-- | Size of public key fingerprint in bytes
fingerprintSize :: forall alg proxy i. (Crypto alg, Num i) => proxy alg -> i
fingerprintSize _ = fromIntegral $ natVal (Proxy :: Proxy (FingerprintSize alg))

-- | Size of public key in bytes
publicKeySize :: forall alg proxy i. (Crypto alg, Num i) => proxy alg -> i
publicKeySize _ = fromIntegral $ natVal (Proxy :: Proxy (PublicKeySize alg))

-- | Size of private key in bytes
privKeySize :: forall alg proxy i. (Crypto alg, Num i) => proxy alg -> i
privKeySize _ = fromIntegral $ natVal (Proxy :: Proxy (PrivKeySize alg))

-- | Size of signature in bytes
signatureSize :: forall alg proxy i. (Crypto alg, Num i) => proxy alg -> i
signatureSize _ = fromIntegral $ natVal (Proxy :: Proxy (SignatureSize alg))

-- | Value could be represented as bytestring.
class (Ord a) => ByteRepr a where
  decodeFromBS :: BS.ByteString -> Maybe a
  encodeToBS   :: a -> BS.ByteString

instance CryptoHash alg => ByteRepr (Hash alg) where
  decodeFromBS            = Just . Hash
  encodeToBS (Hash bs) = bs

instance CryptoSign alg => ByteRepr (Fingerprint alg) where
  decodeFromBS            = Just . Fingerprint
  encodeToBS (Fingerprint bs) = bs

instance CryptoSign alg => ByteRepr (Signature alg) where
  decodeFromBS            = Just . Signature
  encodeToBS (Signature bs) = bs

-- | Encode value as base-58 encoded string
encodeBase58 :: ByteRepr a => a -> Text
encodeBase58 = T.decodeUtf8 . encodeBSBase58 . encodeToBS

-- | Decode value from base-58 encoded string
decodeBase58 :: ByteRepr a => Text -> Maybe a
decodeBase58 = decodeFromBS <=< decodeBSBase58 . T.encodeUtf8


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance CryptoSign alg => Show (PrivKey alg) where
  show = show . BC8.unpack . encodeBSBase58 . encodeToBS

instance CryptoSign alg => Read (PrivKey alg) where
  readPrec = do bs <- readPrecBSBase58
                case decodeFromBS bs of
                  Just k  -> return k
                  Nothing -> empty

instance CryptoSign alg => Serialise (PrivKey alg) where
  encode = CBOR.encode . encodeToBS
  decode = do bs <- CBOR.decode
              case decodeFromBS bs of
                Nothing -> fail "Cannot decode private key"
                Just k  -> return k

instance Crypto alg => SafeCopy (PrivKey alg) where
  kind    = primitive
  putCopy = contain . CBOR.encode
  getCopy = contain   CBOR.decode

instance CryptoSign alg => JSON.ToJSON (PrivKey alg) where
  toJSON = JSON.String . T.decodeUtf8 . encodeBSBase58 . encodeToBS

instance CryptoSign alg => JSON.FromJSON (PrivKey alg) where
  parseJSON (JSON.String s) =
    case decodeBSBase58 $ T.encodeUtf8 s of
      Nothing -> fail  "Incorrect Base58 encoding for PrivKey"
      Just bs -> case decodeFromBS bs of
        Nothing -> fail "Incorrect bytestring representation of PrivKey"
        Just k  -> return k
  parseJSON _          = fail "Expecting PrivKey as string"


----------------------------------------

instance CryptoSign alg => Show (PublicKey alg) where
  show = show . BC8.unpack . encodeBSBase58 . encodeToBS

instance CryptoSign alg => Read (PublicKey alg) where
  readPrec = do bs <- readPrecBSBase58
                case decodeFromBS bs of
                  Just k  -> return k
                  Nothing -> empty

instance CryptoSign alg => Serialise (PublicKey alg) where
  encode = CBOR.encode . encodeToBS
  decode = do bs <- CBOR.decode
              case decodeFromBS bs of
                Nothing -> fail "Cannot decode private key"
                Just k  -> return k

instance CryptoSign alg => JSON.ToJSON (PublicKey alg) where
  toJSON = JSON.String . T.decodeUtf8 . encodeBSBase58 . encodeToBS

instance Crypto alg => SafeCopy (PublicKey alg) where
  kind    = primitive
  putCopy = contain . CBOR.encode
  getCopy = contain   CBOR.decode


instance CryptoSign alg => JSON.FromJSON (PublicKey alg) where
  parseJSON (JSON.String s) =
    case decodeBSBase58 $ T.encodeUtf8 s of
      Nothing -> fail  "Incorrect Base58 encoding for PrivKey"
      Just bs -> case decodeFromBS bs of
        Nothing -> fail "Incorrect bytestring representation of PrivKey"
        Just k  -> return k
  parseJSON _          = fail "Expecting PrivKey as string"


----------------------------------------


instance Show (Fingerprint alg) where
  showsPrec n (Fingerprint bs)
    = showParen (n > 10)
    $ showString "Fingerprint " . shows (encodeBSBase58 bs)

instance Read (Fingerprint alg) where
  readPrec = do void $ lift $ string "Fingerprint" >> some (char ' ')
                Fingerprint <$> readPrecBSBase58

instance JSON.ToJSON (Fingerprint alg) where
  toJSON (Fingerprint s) = JSON.String $ T.decodeUtf8 $ encodeBSBase58 s

instance SafeCopy (Fingerprint alg) where
  kind    = primitive
  putCopy = contain . CBOR.encode
  getCopy = contain   CBOR.decode

instance JSON.FromJSON (Fingerprint alg) where
  parseJSON (JSON.String s) =
    case decodeBSBase58 $ T.encodeUtf8 s of
      Nothing -> fail  "Incorrect Base58 encoding while decoding Fingerprint"
      Just bs -> return $ Fingerprint bs
  parseJSON _ = fail "Expected string for Fingerprint"


----------------------------------------

instance Show (Signature alg) where
  showsPrec n (Signature bs)
    = showParen (n > 10)
    $ showString "Signature " . shows (encodeBSBase58 bs)

instance Read (Signature alg) where
  readPrec = do void $ lift $ string "Signature" >> some (char ' ')
                Signature <$> readPrecBSBase58

instance SafeCopy (Signature alg) where
  kind    = primitive
  putCopy = contain . CBOR.encode
  getCopy = contain   CBOR.decode

instance JSON.ToJSON (Signature alg) where
  toJSON (Signature s) = JSON.String $ T.decodeUtf8 $ encodeBSBase58 s

instance JSON.FromJSON (Signature alg) where
  parseJSON (JSON.String s) =
    case decodeBSBase58 $ T.encodeUtf8 s of
      Nothing -> fail  "Incorrect Base58 encoding while decoding Fingerprint"
      Just bs -> return $ Signature bs
  parseJSON _ = fail "Expected string for Signature"



----------------------------------------

instance CryptoHash alg => Show (Hash alg) where
  showsPrec n h
    = showParen (n > 10)
    $ showString "Hash " . shows (encodeBSBase58 $ encodeToBS h)

instance CryptoHash alg => Read (Hash alg) where
  readPrec = do void $ lift $ string "Hash" >> some (char ' ')
                val <- readPrecBSBase58
                case decodeFromBS val of
                  Nothing -> fail "Incorrect bytestring representation of Hash"
                  Just h  -> return h

instance CryptoHash alg => JSON.ToJSON (Hash alg) where
  toJSON = JSON.String . encodeBase58

instance SafeCopy (Hash alg) where
  kind    = primitive
  putCopy = contain . CBOR.encode
  getCopy = contain   CBOR.decode


instance CryptoHash alg => JSON.FromJSON (Hash alg) where
  parseJSON (JSON.String s) =
    case decodeBase58 s of
      Nothing -> fail  "Incorrect Base58 encoding for bs"
      Just h  -> return h
  parseJSON _ = fail "Expected string for Hash"


----------------------------------------------------------------
-- Petrification of data
----------------------------------------------------------------

-- | Petrified data. When we introduce migrations @encode . decode@ is
--   no longer @id@ in general. So after introduction of new version
--   of data types its serialized form changes and hashes and
--   signatures are no longer valid. To circumvent this problem we
--   carry around serialized representation of data type.
--
--   Only way to petrify data is to call 'petrify'. It should be only
--   called for creation of blocks/transactions.
data Pet a = Pet !a !BS.ByteString
  deriving (Show)

instance Eq a => Eq (Pet a) where
  Pet a _ == Pet b _ = a == b

instance Ord a => Ord (Pet a) where
  compare (Pet a _) (Pet b _) = compare a b

instance NFData a => NFData (Pet a) where
  rnf (Pet a bs) = rnf a `seq` rnf bs

instance SafeCopy a => SafeCopy (Pet a) where
  kind    = primitive
  putCopy = \(Pet _ bs) -> contain $ CBOR.encode bs
  getCopy = contain $ do
    bs <- CBOR.decode
    case safeDecode (fromStrict bs) of
      Left  e -> fail (show e)
      Right a -> return $! Pet a bs

instance JSON.ToJSON (Pet a) where
  toJSON (Pet _ bs) = JSON.String $ T.decodeUtf8 $ encodeBSBase58 bs
instance (SafeCopy a) => JSON.FromJSON (Pet a) where
  parseJSON = JSON.withText "Pet a" $ \s -> 
    case decodeBSBase58 $ T.encodeUtf8 s of
      Nothing -> fail  "Incorrect Base58 encoding while decoding Address"
      Just bs -> case safeDecode (fromStrict bs) of
        Left  e -> fail (show e)
        Right a -> return $! Pet a bs

pet :: Pet a -> a
pet (Pet a _) = a

petrify :: SafeCopy a => a -> Pet a
petrify a = Pet a (toStrict (safeEncode a))


----------------------------------------------------------------
-- Signing and verification of values
----------------------------------------------------------------

-- | Whether signature has been verified or not. Note that all data
--   coming from external sources should be treated as unverified.
data SignedState = Verified
                 | Unverified

-- | Signed value. It contain signature, fingerprint of public key
--   (address) and value itself. Signature is computed for CBOR
--   encoding of value.
data Signed (sign :: SignedState) alg a
  = Signed !(Fingerprint alg) !(Signature alg) !(Pet a)
  deriving (Generic, Eq, Show)

instance (NFData a) => NFData (Signed sign alg a) where
  rnf (Signed a s x) = rnf a `seq` rnf s `seq` rnf x

-- | Obtain underlying value
signedValue :: Signed sign alg a -> a
signedValue (Signed _ _ a) = pet a

signedPetrified :: Signed sign alg a -> Pet a
signedPetrified (Signed _ _ a) = a

-- | Obtain fingerprint used for signing
signedAddr :: Signed sign alg a -> Fingerprint alg
signedAddr (Signed a _ _) = a

-- | Make unverified signature
makeSigned :: Fingerprint alg -> Signature alg -> Pet a -> Signed 'Unverified alg a
makeSigned = Signed

-- | Sign value. Not that we can generate both verified and unverified
--   values this way.
signValue
  :: (CryptoSign alg)
  => PrivKey alg                -- ^ Key for signing
  -> Pet a                      -- ^ Value to sign
  -> Signed sign alg a
signValue privK a@(Pet _ bs)
  = Signed (fingerprint $ publicKey privK)
           (signBlob privK bs)
           a

-- | Verify signature. It return Nothing if verification fails for any
--   reason. Note that since @Signed@ contain only fingerprint we need
--   to supply function for looking up public keys.
verifySignature
  :: (CryptoSign alg)
  => (Fingerprint alg -> Maybe (PublicKey alg))
     -- ^ Lookup function for public keys. If fingerprint is unknown (this
     --   function returns Nothing) verification fails.
  -> Signed 'Unverified alg a
     -- ^ Value for verifying signature
  -> Maybe  (Signed 'Verified alg a)
verifySignature lookupKey s@(Signed addr signature (Pet _ bs)) = do
  pubK <- lookupKey addr
  guard $ verifyBlobSignature pubK bs signature
  return $ coerce s

-- | Strip verification tag
unverifySignature :: Signed ty alg a -> Signed 'Unverified alg a
unverifySignature (Signed addr sig a) = Signed addr sig a


signPetrified
  :: (CryptoSign alg)
  => PrivKey alg
  -> Pet a
  -> Signature alg
signPetrified pk (Pet _ bs) = signBlob pk bs

-- | Verify signature of value.
verifyPetrifiedSignature
  :: (Crypto alg)
  => PublicKey alg
  -> Pet a
  -> Signature alg
  -> Bool
verifyPetrifiedSignature pk (Pet _ bs) = verifyBlobSignature pk bs

instance SafeCopy    a => SafeCopy (Signed 'Unverified alg a) where
  kind = primitive
instance SafeCopy    a => JSON.FromJSON (Signed 'Unverified alg a)
instance JSON.ToJSON a => JSON.ToJSON   (Signed 'Unverified alg a)



----------------------------------------------------------------
-- Hashed data
----------------------------------------------------------------

-- | Newtype wrapper with phantom type tag which show hash of which
--   value is being calculated
newtype Hashed alg a = Hashed (Hash alg)
  deriving ( Show,Eq,Ord, Generic, Generic1, NFData
           , Serialise, JSON.FromJSON,JSON.ToJSON)

instance SafeCopy (Hashed alg a) where
  kind    = primitive
  putCopy = contain . CBOR.encode
  getCopy = contain   CBOR.decode

-- | Compute hash of value. It's first serialized using CBOR and then
--   hash of encoded data is computed,
hash :: (Crypto alg) => Pet a -> Hash alg
hash (Pet _ bs) = hashBlob bs

hashed :: (Crypto alg) => Pet a -> Hashed alg a
hashed = Hashed . hash



----------------------------------------------------------------
-- Base58 encoding helpers
----------------------------------------------------------------

encodeBSBase58 :: BS.ByteString -> BS.ByteString
encodeBSBase58 = Base58.encodeBase58 Base58.bitcoinAlphabet

decodeBSBase58 :: BS.ByteString -> Maybe BS.ByteString
decodeBSBase58 = Base58.decodeBase58 Base58.bitcoinAlphabet

readPrecBSBase58 :: ReadPrec BS.ByteString
readPrecBSBase58 = do
  s <- readPrec
  guard (all isAscii s)
  case decodeBSBase58 $ BC8.pack s of
    Just bs -> return bs
    Nothing -> mzero
