{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
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
module HSChain.Crypto (
    -- * Cryptographic hashes
    Hash(..)
  , Hashed(..)
  , CryptoHash(..)
  , CryptoHashable(..)
  , CryptoTypeHashable(..)
  , DataType(..)
  , Constructor(..)
  , hashBlob
  , hash
  , hashed
  , (:<<<)
    -- * HMAC
  , CryptoHMAC(..)
  , HMAC(..)
    -- ** Sizes
  , hashSize
    -- * Cryptography with asymmetric keys
  , PrivKey
  , PublicKey
  , CryptoAsymmetric(..)
    -- * Signatures API
  , Signature(..)
  , Fingerprint(..)
  , fingerprint
  , CryptoSign(..)
    -- ** Diffie–Hellman key exchange
  , DHSecret
  , CryptoDH(..)
    -- ** Sizes
  , publicKeySize
  , privKeySize
  , signatureSize
  , dhSecretSize
    -- * Convenince: signatures and hashes
  , Crypto
  , (:&)
    -- * Key derivation functions
  , KDFOutput(..)
  , CryptoKDF(..)
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
  , PubKeyBox(..)
  , makePubKeyBox
  , openPubKeyBox
    -- * Encoding and decoding of values
  , ByteRepr(..)
  , ByteReprSized(..)
  , encodeBase58
  , decodeBase58
    -- * Serialization and signatures
  , SignedState(..)
  , verifyCborSignature
  ) where

import Codec.Serialise (Serialise, serialise)
import qualified Codec.Serialise as CBOR
import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Control.Monad.IO.Class

import qualified Data.Aeson           as JSON
import           Data.Data               (Data)
import           Data.ByteString.Lazy    (toStrict)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BC8
import qualified Data.ByteString.Internal as BI
import qualified Data.List.NonEmpty       as NE
import Data.Coerce
import Data.Typeable (Proxy(..))
import Data.Int
import Data.Word
import Text.Read
import Text.ParserCombinators.ReadP
import System.IO.Unsafe
import Foreign.Ptr          (castPtr)
import Foreign.Storable     (Storable(..))
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
                   , JSON.FromJSON, JSON.ToJSON, JSON.ToJSONKey, JSON.FromJSONKey)

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


hashed :: (Crypto alg, CryptoHashable a) => a -> Hashed alg a
hashed = Hashed . hash

instance (CryptoHash alg) => ByteRepr (Hashed alg a) where
  encodeToBS (Hashed h) = encodeToBS h
  decodeFromBS = fmap Hashed . decodeFromBS


-- | Chaining of hash algorithms. For example @SHA256 :<<< SHA256@
--   would mean applying SHA256 twice or @SHA256 :<<< SHA512@ will
--   work as @sha256 . sha512@.
data hashA :<<< hashB

instance ByteReprSized (Hash hashA) => ByteReprSized (Hash (hashA :<<< hashB)) where
  type ByteSize (Hash (hashA :<<< hashB)) = ByteSize (Hash hashA)

instance (CryptoHash hashA, CryptoHash hashB) => CryptoHash (hashA :<<< hashB) where
  newtype HashAccum (hashA :<<< hashB) s = HashAccumChain (HashAccum hashB s)
  newHashAccum    = coerce (newHashAccum    @hashB)
  updateHashAccum = coerce (updateHashAccum @hashB)
  freezeHashAccum (HashAccumChain acc) = do
    Hash h <- freezeHashAccum acc
    return $! coerce (hashBlob h :: Hash hashA)


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
-- Cryptographic hashes
----------------------------------------------------------------

newtype HMAC alg = HMAC (Hash alg)
  deriving stock   ( Show, Read, Generic, Generic1)
  deriving newtype ( Eq, Ord, NFData, Serialise, ByteRepr
                   , JSON.FromJSON, JSON.ToJSON, JSON.ToJSONKey, JSON.FromJSONKey)

-- | Calculate Hash-based Message Authetication Code (HMAC) according
--   to RFC2104
class CryptoHash alg => CryptoHMAC alg where
  hmac :: BS.ByteString
       -- ^ Key. Must be kept secret since leaking will allow attacker
       --   forge HMACs trivially
       -> BS.ByteString
       -- ^ Message for which HMAC is created
       -> HMAC alg

----------------------------------------------------------------
-- Signatures API
----------------------------------------------------------------

-- | Private key
data family PrivKey   alg

-- | Public key
data family PublicKey alg

-- | Cryptographical algorithms with asymmetrical keys. This is base
--   class which doesn;t provide any interesting functionality except
--   for generation of keys and coversion private to public.
class ( ByteReprSized (PublicKey alg)
      , ByteReprSized (PrivKey   alg)
      , Ord (PublicKey alg)
      ) => CryptoAsymmetric alg where
  -- | Compute public key from  private key
  publicKey       :: PrivKey   alg -> PublicKey alg
  -- | Generate new private key
  generatePrivKey :: MonadIO m => m (PrivKey alg)




-- | Signature
newtype Signature alg = Signature BS.ByteString
  deriving stock   (Generic, Generic1)
  deriving newtype (Eq, Ord, Serialise, NFData)

class ( ByteReprSized (Signature   alg)
      , CryptoAsymmetric alg
      ) => CryptoSign alg where
  -- | Sign sequence of bytes
  signBlob            :: PrivKey   alg -> BS.ByteString -> Signature alg
  -- | Check that signature is correct
  verifyBlobSignature :: PublicKey alg -> BS.ByteString -> Signature alg -> Bool

-- | Public key fingerprint (hash of public key)
newtype Fingerprint hash alg = Fingerprint (Hashed hash (PublicKey alg))
  deriving stock   ( Generic)
  deriving newtype ( Eq, Ord, Serialise, NFData, ByteRepr
                   , JSON.FromJSON, JSON.ToJSON, JSON.ToJSONKey, JSON.FromJSONKey)

instance (CryptoHash hash, ByteReprSized (Hash hash)) => ByteReprSized (Fingerprint hash alg) where
  type ByteSize (Fingerprint hash alg) = ByteSize (Hash hash)

-- | Compute fingerprint or public key fingerprint
fingerprint :: (CryptoAsymmetric alg, CryptoHash hash) => PublicKey alg -> Fingerprint hash alg
fingerprint = Fingerprint . Hashed . hashBlob . encodeToBS


-- | Shared secret produced by Diffie-Hellman key exchange algorithm.
data family DHSecret alg

-- | Cryptographical algorithm that support some variant of
--   Diffie-Hellman key exchange
class ( ByteReprSized (DHSecret alg)
      , CryptoAsymmetric alg
      ) => CryptoDH alg where
  -- | Calculate shared secret from private and public key. Following
  --   property must hold:
  --
  --   > diffieHelman (public k1) k2 == diffieHelman (public k2) k1
  diffieHelman :: PublicKey alg -> PrivKey alg -> DHSecret alg


-- | Size of public key in bytes
publicKeySize :: forall alg proxy i. (CryptoAsymmetric alg, Num i) => proxy alg -> i
publicKeySize _ = fromIntegral $ natVal (Proxy @(ByteSize (PublicKey alg)))

-- | Size of private key in bytes
privKeySize :: forall alg proxy i. (CryptoAsymmetric alg, Num i) => proxy alg -> i
privKeySize _ = fromIntegral $ natVal (Proxy @(ByteSize (PrivKey alg)))

-- | Size of signature in bytes
signatureSize :: forall alg proxy i. (CryptoSign alg, Num i) => proxy alg -> i
signatureSize _ = fromIntegral $ natVal (Proxy @(ByteSize (Signature alg)))

-- | Size of signature in bytes
dhSecretSize :: forall alg proxy i. (CryptoDH alg, Num i) => proxy alg -> i
dhSecretSize _ = fromIntegral $ natVal (Proxy @(ByteSize (DHSecret alg)))


----------------------------------------

instance ByteRepr (Signature alg) where
  decodeFromBS              = Just . Signature
  encodeToBS (Signature bs) = bs


----------------------------------------

instance CryptoAsymmetric alg => Show (PrivKey alg) where
  show = defaultShow
instance CryptoAsymmetric alg => Read (PrivKey alg) where
  readPrec = defaultReadPrec

instance CryptoAsymmetric alg => Serialise (PrivKey alg) where
  encode = defaultCborEncode
  decode = defaultCborDecode "PrivKey"

instance CryptoAsymmetric alg => JSON.ToJSON   (PrivKey alg) where
  toJSON    = defaultToJSON
instance CryptoAsymmetric alg => JSON.FromJSON (PrivKey alg) where
  parseJSON = defaultParseJSON "PrivKey"
instance CryptoAsymmetric alg => JSON.FromJSONKey (PrivKey alg)
instance CryptoAsymmetric alg => JSON.ToJSONKey   (PrivKey alg)


----------------------------------------

instance CryptoAsymmetric alg => Show (PublicKey alg) where
  show = defaultShow
instance CryptoAsymmetric alg => Read (PublicKey alg) where
  readPrec = defaultReadPrec

instance CryptoAsymmetric alg => Serialise (PublicKey alg) where
  encode = defaultCborEncode
  decode = defaultCborDecode "PublicKey"

instance CryptoAsymmetric alg => JSON.ToJSON   (PublicKey alg) where
  toJSON    = defaultToJSON
instance CryptoAsymmetric alg => JSON.FromJSON (PublicKey alg) where
  parseJSON = defaultParseJSON "PublicKey"
instance CryptoAsymmetric alg => JSON.FromJSONKey (PublicKey alg)
instance CryptoAsymmetric alg => JSON.ToJSONKey   (PublicKey alg)


----------------------------------------

instance CryptoDH alg => Show (DHSecret alg) where
  show = defaultShow
instance CryptoDH alg => Read (DHSecret alg) where
  readPrec = defaultReadPrec

instance CryptoDH alg => Serialise (DHSecret alg) where
  encode = defaultCborEncode
  decode = defaultCborDecode "DHSecret"

instance CryptoDH alg => JSON.ToJSON   (DHSecret alg) where
  toJSON    = defaultToJSON
instance CryptoDH alg => JSON.FromJSON (DHSecret alg) where
  parseJSON = defaultParseJSON "DHSecret"
instance CryptoDH alg => JSON.FromJSONKey (DHSecret alg)
instance CryptoDH alg => JSON.ToJSONKey   (DHSecret alg)


----------------------------------------

instance Show (Fingerprint hash alg) where
  showsPrec n (Fingerprint (Hashed (Hash bs)))
    = showParen (n > 10)
    $ showString "Fingerprint " . shows (encodeBSBase58 bs)

instance Read (Fingerprint hash alg) where
  readPrec = do void $ lift $ string "Fingerprint" >> some (char ' ')
                coerce <$> readPrecBSBase58

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

instance ByteReprSized (PublicKey sign) => ByteReprSized (PublicKey (sign :& hash)) where
  type ByteSize (PublicKey (sign :& hash)) = ByteSize (PublicKey sign)
instance ByteReprSized (PrivKey sign) => ByteReprSized (PrivKey (sign :& hash)) where
  type ByteSize (PrivKey (sign :& hash)) = ByteSize (PrivKey sign)

instance CryptoAsymmetric sign => CryptoAsymmetric (sign :& hash) where
  publicKey       = coerce (publicKey @sign)
  generatePrivKey = fmap PrivKeyU (generatePrivKey @sign)

instance ByteReprSized (Signature sign) => ByteReprSized (Signature (sign :& hash)) where
  type ByteSize (Signature (sign :& hash)) = ByteSize (Signature sign)

instance CryptoSign sign => CryptoSign (sign :& hash) where
  signBlob            = coerce (signBlob @sign)
  verifyBlobSignature = coerce (verifyBlobSignature @sign)

instance ByteReprSized (Hash hash) => ByteReprSized (Hash (sign :& hash)) where
  type ByteSize (Hash (sign :& hash)) = ByteSize (Hash hash)

instance (CryptoHash hash) => CryptoHash (sign :& hash) where
  newtype HashAccum (sign :& hash) s = HashAccumBoth (HashAccum hash s)
  newHashAccum    = coerce (newHashAccum    @hash)
  updateHashAccum = coerce (updateHashAccum @hash)
  freezeHashAccum = coerce (freezeHashAccum @hash)



----------------------------------------------------------------
-- Key derivation functions
----------------------------------------------------------------

-- | Output of key derivation functions
newtype KDFOutput alg = KDFOutput BS.ByteString

-- | Type class for key derivation functions.
class (ByteReprSized (KDFOutput alg)) => CryptoKDF alg where
  -- | Extra parameters such as nonce, number of iterations etc.
  type KDFParams alg
  -- | Generate random sequence of bytes
  deriveKey :: KDFParams alg -> BS.ByteString -> KDFOutput alg

instance ByteRepr (KDFOutput alg) where
  encodeToBS (KDFOutput h) = encodeToBS h
  decodeFromBS = fmap KDFOutput . decodeFromBS


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
class ( ByteReprSized (CypherKey       alg)
      , ByteReprSized (CypherNonce     alg)
      ) => StreamCypher alg where
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


-- | Value ecrypted using public key cryptography
data PubKeyBox key kdf cypher = PubKeyBox
  { pubKeyBoxValue :: !BS.ByteString
  , pubKeyNonce    :: !(CypherNonce cypher)
  }

makePubKeyBox
  :: forall m key kdf cypher.
     ( MonadIO m
     , CryptoDH key, CryptoKDF kdf, StreamCypher cypher
     , ByteSize (KDFOutput kdf) ~ ByteSize (CypherKey cypher)
     , KDFParams kdf            ~ ()
     )
  => PrivKey   key              -- ^ Our private key
  -> PublicKey key              -- ^ Public key of repicient or sender
  -> BS.ByteString              -- ^ Clear text message
  -> m (PubKeyBox key kdf cypher)
makePubKeyBox privK pubK msg = do
  let sharedSecret     = diffieHelman pubK privK
      KDFOutput kdf    = deriveKey () $ encodeToBS sharedSecret :: KDFOutput kdf
      Just derivedKey  = decodeFromBS kdf
  nonce <- generateCypherNonce
  return $! PubKeyBox { pubKeyBoxValue = encryptMessage derivedKey nonce msg
                      , pubKeyNonce    = nonce
                      }

openPubKeyBox
  :: forall key kdf cypher.
     ( CryptoDH key, CryptoKDF kdf, StreamCypher cypher
     , ByteSize (KDFOutput kdf) ~ ByteSize (CypherKey cypher)
     , KDFParams kdf            ~ ()
     )
  => PrivKey   key              -- ^ Our private key
  -> PublicKey key              -- ^ Public key of repicient or sender
  -> PubKeyBox key kdf cypher
  -> Maybe BS.ByteString
openPubKeyBox privK pubK box = do
  let sharedSecret     = diffieHelman pubK privK
      KDFOutput kdf    = deriveKey () $ encodeToBS sharedSecret :: KDFOutput kdf
      Just derivedKey  = decodeFromBS kdf
  decryptMessage derivedKey (pubKeyNonce box) (pubKeyBoxValue box)


-- | Size of key of cyper algorithm
cypherKeySize :: forall alg proxy i. (StreamCypher alg, Num i) => proxy alg -> i
cypherKeySize _ = fromIntegral $ natVal (Proxy @(ByteSize (CypherKey alg)))

-- | Size of nonce of cyper algorithm
cypherNonceSize :: forall alg proxy i. (StreamCypher alg, Num i) => proxy alg -> i
cypherNonceSize _ = fromIntegral $ natVal (Proxy @(ByteSize (CypherNonce alg)))


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
--   coming from external sources should be treated as unverified
--   therefore only @Signed 'Unverified@ has instances for
--   serialization
data SignedState = Verified
                 | Unverified

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

instance CryptoHashable ByteString where
  hashStep = updateHashAccum

----------------------------------------
-- Normal data types

instance CryptoHashable a => CryptoHashable [a] where
  hashStep s xs = do hashStep s $ Sequence $ fromIntegral $ length xs
                     mapM_ (hashStep s) xs

instance CryptoHashable a => CryptoHashable (NE.NonEmpty a) where
  hashStep s xs = do hashStep s $ Sequence $ fromIntegral $ length xs
                     mapM_ (hashStep s) xs


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

instance CryptoHashable (Hashed alg a) where
  hashStep s (Hashed h) = hashStep s h

instance CryptoHashable (Fingerprint hash alg) where
  hashStep s (Fingerprint h) = hashStep s h

instance CryptoHashable (Hash alg) where
  hashStep = undefined

instance CryptoHashable (PublicKey alg) where
  hashStep = undefined

instance CryptoHashable (PrivKey alg) where
  hashStep = undefined

instance CryptoHashable (Signature alg) where
  hashStep = undefined

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
