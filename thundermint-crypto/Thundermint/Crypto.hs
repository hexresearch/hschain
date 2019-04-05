{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
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
  , hash
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
    -- * Encoding and decoding of values
  , ByteRepr(..)
  , encodeBase58
  , decodeBase58
    -- * Serialization and signatures
  , SignedState(..)
  , Signed
  , makeSigned
  , signedValue
  , signedAddr
  , signValue
  , verifySignature
  , unverifySignature
  , verifyCborSignature
    -- * Hash trees
  , Hashed(..)
  , hashed
    -- * base58 encoding
  , encodeBSBase58
  , decodeBSBase58
  , readPrecBSBase58
  ) where

import Codec.Serialise (Serialise, serialise)
import qualified Codec.Serialise as CBOR
import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Aeson           as JSON
import Data.Data (Data)
import           Data.Text              (Text)
import qualified Data.Text.Encoding   as T
import           Data.ByteString.Lazy    (toStrict)
import qualified Data.ByteString.Char8 as BC8
import Data.Coerce
import Data.Char     (isAscii)
import Data.Typeable (Proxy(..))
import Text.Read
import Text.ParserCombinators.ReadP
import GHC.TypeNats
import GHC.Generics         (Generic,Generic1)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base58 as Base58


----------------------------------------------------------------
-- Basic crypto API
----------------------------------------------------------------

type Crypto (alg) = (CryptoSign alg, CryptoHash alg)

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

-- | Compute hash of value. It's first serialized using CBOR and then
--   hash of encoded data is computed,
hash :: (Crypto alg, Serialise a) => a -> Hash alg
hash = hashBlob . toStrict . serialise

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
  -- | Generate new private key
  generatePrivKey     :: MonadIO m => m (PrivKey alg)

class ( KnownNat (SignatureSize alg)
      , KnownNat (FingerprintSize alg)
      , KnownNat (PublicKeySize alg)
      , KnownNat (PrivKeySize alg)
      ) => CryptoSignPrim alg where
  type FingerprintSize alg :: Nat
  type PublicKeySize   alg :: Nat
  type PrivKeySize     alg :: Nat
  type SignatureSize   alg :: Nat

-- | Type-indexed set of crypto algorithms. It's not very principled
--   to push everything into singe type class.  But in order to keep
--   signatures sane it was done this way.
class ( ByteRepr (Hash   alg)
      , KnownNat (HashSize alg)
      ) => CryptoHash alg where
  type HashSize        alg :: Nat
  -- | Compute hash of sequence of bytes
  hashBlob     :: BS.ByteString -> Hash alg
  -- | Compare hash with a bytestring safly
  hashEquality :: Hash alg -> BS.ByteString -> Bool



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
class ByteRepr a where
  decodeFromBS :: BS.ByteString -> Maybe a
  encodeToBS   :: a -> BS.ByteString

instance ByteRepr BS.ByteString where
  decodeFromBS = Just
  encodeToBS   = id

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


instance CryptoSign alg => Serialise (PublicKey alg) where
  encode = CBOR.encode . encodeToBS
  decode = do bs <- CBOR.decode
              case decodeFromBS bs of
                Nothing -> fail "Cannot decode private key"
                Just k  -> return k

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

instance CryptoSign alg => JSON.FromJSONKey (PrivKey alg)
instance CryptoSign alg => JSON.ToJSONKey   (PrivKey alg)


----------------------------------------

instance CryptoSign alg => Show (PublicKey alg) where
  show = show . BC8.unpack . encodeBSBase58 . encodeToBS

instance CryptoSign alg => Read (PublicKey alg) where
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

instance CryptoSign alg => JSON.ToJSON (PublicKey alg) where
  toJSON = JSON.String . T.decodeUtf8 . encodeBSBase58 . encodeToBS

instance CryptoSign alg => JSON.FromJSON (PublicKey alg) where
  parseJSON (JSON.String s) =
    case decodeBSBase58 $ T.encodeUtf8 s of
      Nothing -> fail  "Incorrect Base58 encoding for PrivKey"
      Just bs -> case decodeFromBS bs of
        Nothing -> fail "Incorrect bytestring representation of PrivKey"
        Just k  -> return k
  parseJSON _          = fail "Expecting PrivKey as string"

instance CryptoSign alg => JSON.FromJSONKey (PublicKey alg)
instance CryptoSign alg => JSON.ToJSONKey   (PublicKey alg)


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

instance JSON.FromJSON (Fingerprint alg) where
  parseJSON (JSON.String s) =
    case decodeBSBase58 $ T.encodeUtf8 s of
      Nothing -> fail  "Incorrect Base58 encoding while decoding Fingerprint"
      Just bs -> return $ Fingerprint bs
  parseJSON _ = fail "Expected string for Fingerprint"

instance JSON.FromJSONKey (Fingerprint alg)
instance JSON.ToJSONKey   (Fingerprint alg)


----------------------------------------

instance Show (Signature alg) where
  showsPrec n (Signature bs)
    = showParen (n > 10)
    $ showString "Signature " . shows (encodeBSBase58 bs)

instance Read (Signature alg) where
  readPrec = do void $ lift $ string "Signature" >> some (char ' ')
                Signature <$> readPrecBSBase58

instance JSON.ToJSON (Signature alg) where
  toJSON (Signature s) = JSON.String $ T.decodeUtf8 $ encodeBSBase58 s

instance JSON.FromJSON (Signature alg) where
  parseJSON (JSON.String s) =
    case decodeBSBase58 $ T.encodeUtf8 s of
      Nothing -> fail  "Incorrect Base58 encoding while decoding Fingerprint"
      Just bs -> return $ Signature bs
  parseJSON _ = fail "Expected string for Signature"

instance JSON.FromJSONKey (Signature alg)
instance JSON.ToJSONKey   (Signature alg)


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

instance CryptoHash alg => JSON.FromJSON (Hash alg) where
  parseJSON (JSON.String s) =
    case decodeBase58 s of
      Nothing -> fail  "Incorrect Base58 encoding for bs"
      Just h  -> return h
  parseJSON _ = fail "Expected string for Hash"

instance CryptoHash alg => JSON.FromJSONKey (Hash alg)
instance CryptoHash alg => JSON.ToJSONKey   (Hash alg)


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
  = Signed !(Fingerprint alg) !(Signature alg) !a
  deriving (Generic, Eq, Show)

instance (NFData a) => NFData (Signed sign alg a) where
  rnf (Signed a s x) = rnf a `seq` rnf s `seq` rnf x

-- | Obtain underlying value
signedValue :: Signed sign alg a -> a
signedValue (Signed _ _ a) = a

-- | Obtain fingerprint used for signing
signedAddr :: Signed sign alg a -> Fingerprint alg
signedAddr (Signed a _ _) = a

-- | Make unverified signature
makeSigned :: Fingerprint alg -> Signature alg -> a -> Signed 'Unverified alg a
makeSigned = Signed

-- | Sign value. Not that we can generate both verified and unverified
--   values this way.
signValue
  :: (Serialise a, CryptoSign alg)
  => PrivKey alg                -- ^ Key for signing
  -> a                          -- ^ Value to sign
  -> Signed sign alg a
signValue privK a
  = Signed (fingerprint $ publicKey privK)
           (signBlob privK $ toStrict $ serialise a)
           a

-- | Verify signature. It return Nothing if verification fails for any
--   reason. Note that since @Signed@ contain only fingerprint we need
--   to supply function for looking up public keys.
verifySignature
  :: (Serialise a, CryptoSign alg)
  => (Fingerprint alg -> Maybe (PublicKey alg))
     -- ^ Lookup function for public keys. If fingerprint is unknown (this
     --   function returns Nothing) verification fails.
  -> Signed 'Unverified alg a
     -- ^ Value for verifying signature
  -> Maybe  (Signed 'Verified alg a)
verifySignature lookupKey (Signed addr signature a) = do
  pubK <- lookupKey addr
  guard $ verifyCborSignature pubK a signature
  return $ Signed addr signature a

-- | Strip verification tag
unverifySignature :: Signed ty alg a -> Signed 'Unverified alg a
unverifySignature (Signed addr sig a) = Signed addr sig a

-- | Verify signature of value. Signature is verified for CBOR
--   encoding of object
verifyCborSignature
  :: (Serialise a, CryptoSign alg)
  => PublicKey alg
  -> a
  -> Signature alg
  -> Bool
verifyCborSignature pk a
  = verifyBlobSignature pk (toStrict $ serialise a)

instance Serialise a => Serialise (Signed 'Unverified alg a)
instance JSON.FromJSON a => JSON.FromJSON (Signed 'Unverified alg a)
instance JSON.ToJSON   a => JSON.ToJSON   (Signed 'Unverified alg a)


----------------------------------------------------------------
-- Union of hashing and signing
----------------------------------------------------------------

-- | Data type which support both hashing and signing
data sign :& hash
  deriving (Data)


newtype instance PrivKey   (sign :& hash) = PrivKeyU   (PrivKey   sign)
newtype instance PublicKey (sign :& hash) = PublicKeyU (PublicKey sign)

deriving instance (Eq     (PrivKey   sign)) => Eq     (PrivKey   (sign :& hash))
deriving instance (Ord    (PrivKey   sign)) => Ord    (PrivKey   (sign :& hash))
deriving instance (NFData (PrivKey   sign)) => NFData (PrivKey   (sign :& hash))
deriving instance (Eq     (PublicKey sign)) => Eq     (PublicKey (sign :& hash))
deriving instance (Ord    (PublicKey sign)) => Ord    (PublicKey (sign :& hash))
deriving instance (NFData (PublicKey sign)) => NFData (PublicKey (sign :& hash))

instance (ByteRepr (PrivKey sign)) => ByteRepr (PrivKey (sign :& hash)) where
  encodeToBS   = coerce (encodeToBS   @(PrivKey sign))
  decodeFromBS = coerce (decodeFromBS @(PrivKey sign))

instance (ByteRepr (PublicKey sign)) => ByteRepr (PublicKey (sign :& hash)) where
  encodeToBS   = coerce (encodeToBS   @(PublicKey sign))
  decodeFromBS = coerce (decodeFromBS @(PublicKey sign))

instance CryptoSign sign => CryptoSign (sign :& hash) where
  signBlob            = coerce (signBlob @sign)
  verifyBlobSignature = coerce (verifyBlobSignature @sign)
  publicKey           = coerce (publicKey @sign)
  fingerprint         = coerce (fingerprint @sign)
  generatePrivKey     = fmap PrivKeyU (generatePrivKey @sign)

instance (CryptoSignPrim sign) => CryptoSignPrim (sign :& hash) where
  type FingerprintSize (sign :& hash) = FingerprintSize sign
  type PublicKeySize   (sign :& hash) = PublicKeySize   sign
  type PrivKeySize     (sign :& hash) = PrivKeySize     sign
  type SignatureSize   (sign :& hash) = SignatureSize   sign

instance (CryptoHash hash) => CryptoHash (sign :& hash) where
  type HashSize (sign :& hash) = HashSize hash
  hashBlob     = coerce (hashBlob @hash)
  hashEquality = coerce (hashEquality @hash)


----------------------------------------------------------------
-- Hashed data
----------------------------------------------------------------

-- | Newtype wrapper with phantom type tag which show hash of which
--   value is being calculated
newtype Hashed alg a = Hashed (Hash alg)
  deriving ( Show,Read, Eq,Ord, Generic, Generic1, NFData
           , Serialise, JSON.FromJSON, JSON.ToJSON, JSON.ToJSONKey, JSON.FromJSONKey)

hashed :: (Crypto alg, Serialise a) => a -> Hashed alg a
hashed = Hashed . hash

instance (CryptoHash alg) => ByteRepr (Hashed alg a) where
  encodeToBS (Hashed h) = encodeToBS h
  decodeFromBS = fmap Hashed . decodeFromBS

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
