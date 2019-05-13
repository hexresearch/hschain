{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
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
    -- * Cryptographic hashes
    CryptoHash(..)
  , Hash(..)
  , Hashed(..)
  , hash
  , hashed
  , (:<<<)
    -- ** Sizes
  , hashSize
    -- * Signatures API
  , PrivKey
  , PublicKey
  , Signature(..)
  , Fingerprint(..)
  , CryptoSign(..)
  , CryptoSignPrim(..)
    -- ** Sizes
  , fingerprintSize
  , publicKeySize
  , privKeySize
  , signatureSize
    -- * Convenince: signatures and hashes
  , Crypto
  , (:&)
    -- * Stream cyphers
  , CypherKey
  , CypherNonce
  , StreamCypher(..)
  , cypherKeySize
  , cypherNonceSize
    -- ** Higher-level API
  , SecretBox(..)
  , makeSecretBox
  , openSecretBox
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
  ) where

import Codec.Serialise (Serialise, serialise)
import qualified Codec.Serialise as CBOR
import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Aeson           as JSON
import           Data.Data (Data)
import           Data.ByteString.Lazy    (toStrict)
import qualified Data.ByteString       as BS
import Data.Coerce
import Data.Typeable (Proxy(..))
import Text.Read
import Text.ParserCombinators.ReadP
import GHC.TypeNats
import GHC.Generics         (Generic,Generic1)

import Thundermint.Crypto.Classes


----------------------------------------------------------------
-- Cryptographic hashes
----------------------------------------------------------------

-- | Cryptographic hash of some value
newtype Hash alg = Hash BS.ByteString
  deriving stock   (Generic, Generic1)
  deriving newtype (Eq,Ord,Serialise,NFData)

-- | Compute hash of value. It's first serialized using CBOR and then
--   hash of encoded data is computed,
hash :: (CryptoHash alg, Serialise a) => a -> Hash alg
hash = hashBlob . toStrict . serialise

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
hashSize :: forall alg proxy i. (CryptoHash alg, Num i) => proxy alg -> i
hashSize _ = fromIntegral $ natVal (Proxy :: Proxy (HashSize alg))

-- | Newtype wrapper with phantom type tag which show hash of which
--   value is being calculated
newtype Hashed alg a = Hashed (Hash alg)
  deriving stock   ( Show, Read, Generic, Generic1)
  deriving newtype ( Eq,Ord,NFData, Serialise
                   , JSON.FromJSON, JSON.ToJSON, JSON.ToJSONKey, JSON.FromJSONKey)

hashed :: (Crypto alg, Serialise a) => a -> Hashed alg a
hashed = Hashed . hash

instance (CryptoHash alg) => ByteRepr (Hashed alg a) where
  encodeToBS (Hashed h) = encodeToBS h
  decodeFromBS = fmap Hashed . decodeFromBS

-- | Chaining of hash algorithms. For example @SHA256 :<<< SHA256@
--   would mean applying SHA256 twice or @SHA256 :<<< SHA512@ will
--   work as @sha256 . sha512@.
data hashA :<<< hashB

instance (CryptoHash hashA, CryptoHash hashB) => CryptoHash (hashA :<<< hashB) where
  type HashSize (hashA :<<< hashB) = HashSize hashA
  hashBlob bs = let Hash hB = hashBlob bs :: Hash hashB
                    Hash hA = hashBlob hB :: Hash hashA
                in Hash hA
  hashEquality (Hash hbs) bs = hbs == bs


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
-- Signatures API
----------------------------------------------------------------

-- | Private key
data family PrivKey   alg

-- | Public key
data family PublicKey alg

-- | Signature
newtype Signature alg = Signature BS.ByteString
  deriving stock   (Generic, Generic1)
  deriving newtype (Eq, Ord, Serialise, NFData)

-- | Public key fingerprint (hash of public key)
newtype Fingerprint alg = Fingerprint BS.ByteString
  deriving stock   (Generic, Generic1)
  deriving newtype (Eq, Ord, Serialise, NFData)

class ( ByteRepr (PublicKey   alg)
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


-- | Size of public key fingerprint in bytes
fingerprintSize :: forall alg proxy i. (CryptoSign alg, Num i) => proxy alg -> i
fingerprintSize _ = fromIntegral $ natVal (Proxy :: Proxy (FingerprintSize alg))

-- | Size of public key in bytes
publicKeySize :: forall alg proxy i. (CryptoSign alg, Num i) => proxy alg -> i
publicKeySize _ = fromIntegral $ natVal (Proxy :: Proxy (PublicKeySize alg))

-- | Size of private key in bytes
privKeySize :: forall alg proxy i. (CryptoSign alg, Num i) => proxy alg -> i
privKeySize _ = fromIntegral $ natVal (Proxy :: Proxy (PrivKeySize alg))

-- | Size of signature in bytes
signatureSize :: forall alg proxy i. (CryptoSign alg, Num i) => proxy alg -> i
signatureSize _ = fromIntegral $ natVal (Proxy :: Proxy (SignatureSize alg))


----------------------------------------

instance ByteRepr (Fingerprint alg) where
  decodeFromBS                = Just . Fingerprint
  encodeToBS (Fingerprint bs) = bs

instance ByteRepr (Signature alg) where
  decodeFromBS              = Just . Signature
  encodeToBS (Signature bs) = bs


----------------------------------------

instance CryptoSign alg => Show (PrivKey alg) where
  show = defaultShow
instance CryptoSign alg => Read (PrivKey alg) where
  readPrec = defaultReadPrec

instance CryptoSign alg => Serialise (PrivKey alg) where
  encode = defaultCborEncode
  decode = defaultCborDecode "PrivKey"

instance CryptoSign alg => JSON.ToJSON   (PrivKey alg) where
  toJSON    = defaultToJSON
instance CryptoSign alg => JSON.FromJSON (PrivKey alg) where
  parseJSON = defaultParseJSON "PrivKey"
instance CryptoSign alg => JSON.FromJSONKey (PrivKey alg)
instance CryptoSign alg => JSON.ToJSONKey   (PrivKey alg)


----------------------------------------

instance CryptoSign alg => Show (PublicKey alg) where
  show = defaultShow
instance CryptoSign alg => Read (PublicKey alg) where
  readPrec = defaultReadPrec

instance CryptoSign alg => Serialise (PublicKey alg) where
  encode = defaultCborEncode
  decode = defaultCborDecode "PublicKey"

instance CryptoSign alg => JSON.ToJSON   (PublicKey alg) where
  toJSON    = defaultToJSON
instance CryptoSign alg => JSON.FromJSON (PublicKey alg) where
  parseJSON = defaultParseJSON "PublicKey"
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

instance JSON.ToJSON   (Fingerprint alg) where
  toJSON    = defaultToJSON
instance JSON.FromJSON (Fingerprint alg) where
  parseJSON = defaultParseJSON "Fingerprint"
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

instance JSON.ToJSON   (Signature alg) where
  toJSON    = defaultToJSON
instance JSON.FromJSON (Signature alg) where
  parseJSON = defaultParseJSON "Signature"
instance JSON.FromJSONKey (Signature alg)
instance JSON.ToJSONKey   (Signature alg)


----------------------------------------------------------------
-- Convenience: Signatures & hashes
----------------------------------------------------------------

type Crypto (alg) = (CryptoSign alg, CryptoHash alg)

-- | Data type which support both hashing and signing
data sign :& hash
  deriving stock (Data)

newtype instance PrivKey   (sign :& hash) = PrivKeyU   (PrivKey   sign)
newtype instance PublicKey (sign :& hash) = PublicKeyU (PublicKey sign)

deriving         instance (Eq     (PrivKey   sign)) => Eq     (PrivKey   (sign :& hash))
deriving         instance (Ord    (PrivKey   sign)) => Ord    (PrivKey   (sign :& hash))
deriving newtype instance (NFData (PrivKey   sign)) => NFData (PrivKey   (sign :& hash))
deriving         instance (Eq     (PublicKey sign)) => Eq     (PublicKey (sign :& hash))
deriving         instance (Ord    (PublicKey sign)) => Ord    (PublicKey (sign :& hash))
deriving newtype instance (NFData (PublicKey sign)) => NFData (PublicKey (sign :& hash))

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
-- Stream cyphers
----------------------------------------------------------------

-- | Key for cypher algorithm
data family CypherKey alg

-- | Nonce for stream cypher. They operate by generating random stream
--   of bytes from key and nonce and xoring cleartext with it. Nonce
--   is required to protect against key reuse attack and same key
--   nonce pair should never be reused. Otherwise it's possible to
--   recover @A xor B@ by xoring cyphertexts created with same key-nonce pair.
data family CypherNonce alg


-- | High level API for stream cypher. In order to protect against
--   bit-flipping attack ecrypted message must contain MAC (message
--   authentication code).
--
--   Note that to protect against reused key attacks same pair key,
--   nonce MUST NOT be used for more than one message.
class ( ByteRepr (CypherKey       alg)
      , ByteRepr (CypherNonce     alg)
      , KnownNat (CypherKeySize   alg)
      , KnownNat (CypherNonceSize alg)
      ) => StreamCypher alg where
  type family CypherNonceSize alg :: Nat
  type family CypherKeySize   alg :: Nat
  -- | Encrypt message. Note that same nonce MUST NOT be reused.
  encryptMessage :: CypherKey alg -> CypherNonce alg -> BS.ByteString -> BS.ByteString
  -- | Decrypt message. If MAC verification fails (message was
  --   tampered with) decription returns @Nothing@
  decryptMessage :: CypherKey alg -> CypherNonce alg -> BS.ByteString -> Maybe BS.ByteString
  -- | Generate random key using cryptographically secure RNG
  generateCypherKey :: MonadIO m => m (CypherKey alg)
  -- | Generate random nonce using cryptographically secure RNG
  generateCypherNonce :: MonadIO m => m (CypherNonce alg)


-- | Value encrypted with stream cypher. It should be created by
--   'makeSecretBox' which takes care of generating nonce.
data SecretBox alg = SecretBox
  { secretBoxValue :: !BS.ByteString
  , secretBoxNonce :: !(CypherNonce alg)
  }
  deriving stock    (Show, Generic)
  deriving anyclass (CBOR.Serialise)

-- | Encrypt value and generate random nonce. This method should be
--   used only if nonce is big enough for collision probability to be
--   negligible.
makeSecretBox :: (MonadIO m, StreamCypher alg) => CypherKey alg -> BS.ByteString -> m (SecretBox alg)
makeSecretBox key msg = do
  nonce <- generateCypherNonce
  return $! SecretBox { secretBoxValue = encryptMessage key nonce msg
                      , secretBoxNonce = nonce
                      }

-- | Decrypt secretbox. It will return @Nothing@ if MAC verification
--   fails. That mean either that message was encrypted.
openSecretBox :: (StreamCypher alg) => CypherKey alg -> SecretBox alg -> Maybe BS.ByteString
openSecretBox key box = decryptMessage key (secretBoxNonce box) (secretBoxValue box)

-- | Size of key of cyper algorithm
cypherKeySize :: forall alg proxy i. (StreamCypher alg, Num i) => proxy alg -> i
cypherKeySize _ = fromIntegral $ natVal (Proxy @(CypherKeySize alg))

-- | Size of nonce of cyper algorithm
cypherNonceSize :: forall alg proxy i. (StreamCypher alg, Num i) => proxy alg -> i
cypherNonceSize _ = fromIntegral $ natVal (Proxy @(CypherNonceSize alg))


----------------------------------------

instance StreamCypher alg => Show (CypherKey alg) where
  show = defaultShow
instance StreamCypher alg => Read (CypherKey alg) where
  readPrec = defaultReadPrec

instance StreamCypher alg => Serialise (CypherKey alg) where
  encode = defaultCborEncode
  decode = defaultCborDecode "CypherKey"

instance StreamCypher alg => JSON.ToJSON   (CypherKey alg) where
  toJSON    = defaultToJSON
instance StreamCypher alg => JSON.FromJSON (CypherKey alg) where
  parseJSON = defaultParseJSON "CypherKey"
instance StreamCypher alg => JSON.FromJSONKey (CypherKey alg)
instance StreamCypher alg => JSON.ToJSONKey   (CypherKey alg)


----------------------------------------

instance StreamCypher alg => Show (CypherNonce alg) where
  show = defaultShow
instance StreamCypher alg => Read (CypherNonce alg) where
  readPrec = defaultReadPrec

instance StreamCypher alg => Serialise (CypherNonce alg) where
  encode = defaultCborEncode
  decode = defaultCborDecode "CypherNonce"

instance StreamCypher alg => JSON.ToJSON   (CypherNonce alg) where
  toJSON    = defaultToJSON
instance StreamCypher alg => JSON.FromJSON (CypherNonce alg) where
  parseJSON = defaultParseJSON "CypherNonce"
instance StreamCypher alg => JSON.FromJSONKey (CypherNonce alg)
instance StreamCypher alg => JSON.ToJSONKey   (CypherNonce alg)



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
  deriving stock (Generic, Eq, Show)

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
