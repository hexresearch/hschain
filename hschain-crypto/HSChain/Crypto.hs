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
-- Type class based API for cryptographic operations where types
-- describe cryprographic algorithms and data types such as
-- public\/private keys, etc are implemented as data families
module HSChain.Crypto (
    -- * Cryptographic hashes
    Hash(..)
  , Hashed(..)
  , CryptoHash(..)
  , CryptoHashable(..)
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
  , CryptoSign(..)
  , signHashed
  , verifySignatureHashed
  , CryptoSignHashed(..)
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
  ) where

import Codec.Serialise (Serialise)
import qualified Codec.Serialise as CBOR
import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Aeson           as JSON
import           Data.Data               (Data)
import           Data.ByteString         (ByteString)
import Data.Coerce
import Data.Typeable (Proxy(..))
import Text.Read
import Text.ParserCombinators.ReadP
import GHC.TypeNats
import GHC.Generics (Generic,Generic1)

import HSChain.Crypto.Classes
import HSChain.Crypto.Classes.Hash




-- | Chaining of hash algorithms. For example @SHA256 :<<< SHA256@
--   would mean applying SHA256 twice or @SHA256 :<<< SHA512@ will
--   work as @sha256 . sha512@.
data hashA :<<< hashB

instance ByteReprSized (Hash hashA) => ByteReprSized (Hash (hashA :<<< hashB)) where
  type ByteSize (Hash (hashA :<<< hashB)) = ByteSize (Hash hashA)

instance (CryptoHash hashA, CryptoHash hashB) => CryptoHash (hashA :<<< hashB) where
  hashBlob     bs = let Hash hB = hashBlob bs :: Hash hashB
                        Hash hA = hashBlob hB :: Hash hashA
                    in  Hash hA
  hashLazyBlob bs = let Hash hB = hashLazyBlob bs :: Hash hashB
                        Hash hA = hashBlob hB     :: Hash hashA
                    in  Hash hA
  hashAlgorithmName = CryptoName $ bsA <> " <<< " <> bsB
    where CryptoName bsA = hashAlgorithmName @hashA
          CryptoName bsB = hashAlgorithmName @hashB

----------------------------------------------------------------
-- Cryptographic hashes
----------------------------------------------------------------

-- | Hash-based Message Authetication Code (HMAC) according to RFC2104.
newtype HMAC alg = HMAC (Hash alg)
  deriving stock   ( Show, Read, Generic, Generic1)
  deriving newtype ( Eq, Ord, NFData, Serialise, ByteRepr
                   , JSON.FromJSON, JSON.ToJSON, JSON.ToJSONKey, JSON.FromJSONKey)

-- | Calculate Hash-based Message Authetication Code (HMAC) according
--   to RFC2104
class CryptoHash alg => CryptoHMAC alg where
  hmac :: ByteString
       -- ^ Key. Must be kept secret since leaking will allow attacker
       --   forge HMACs trivially
       -> ByteString
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
  publicKey       :: PrivKey alg -> PublicKey alg
  -- | Generate new private key
  generatePrivKey :: MonadIO m => m (PrivKey alg)
  asymmKeyAlgorithmName :: CryptoName alg


-- | Signature
newtype Signature alg = Signature ByteString
  deriving stock   (Generic, Generic1)
  deriving newtype (Eq, Ord, Serialise, NFData)


class ( ByteReprSized (Signature   alg)
      , CryptoAsymmetric alg
      ) => CryptoSign alg where
  -- | Sign sequence of bytes
  signBlob            :: PrivKey   alg -> ByteString -> Signature alg
  -- | Check that signature is correct
  verifyBlobSignature :: PublicKey alg -> ByteString -> Signature alg -> Bool


signHashed :: forall alg a. (Crypto alg, CryptoHashable a)
           => PrivKey alg -> a -> Signature alg
{-# INLINABLE signHashed #-}
signHashed pk a = signBlob pk bs
  where
    Hash bs = hash a :: Hash alg

verifySignatureHashed :: forall alg a. (Crypto alg, CryptoHashable a)
           => PublicKey alg -> a -> Signature alg -> Bool
{-# INLINABLE verifySignatureHashed #-}
verifySignatureHashed pk a = verifyBlobSignature pk bs
  where
    Hash bs = hash a :: Hash alg

class ( ByteReprSized (Signature   alg)
      , CryptoAsymmetric alg
      ) => CryptoSignHashed alg where
  -- | Sign hashed sequence of bytes
  signHash            :: PrivKey   alg -> Hash alg -> Signature alg
  -- | Check that signature is correct
  verifyHashSignature :: PublicKey alg -> Hash alg -> Signature alg -> Bool


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
  publicKey             = coerce (publicKey @sign)
  generatePrivKey       = fmap PrivKeyU (generatePrivKey @sign)
  asymmKeyAlgorithmName = coerce (asymmKeyAlgorithmName @sign)


instance ByteReprSized (Signature sign) => ByteReprSized (Signature (sign :& hash)) where
  type ByteSize (Signature (sign :& hash)) = ByteSize (Signature sign)

instance CryptoSign sign => CryptoSign (sign :& hash) where
  signBlob            = coerce (signBlob @sign)
  verifyBlobSignature = coerce (verifyBlobSignature @sign)

instance ByteReprSized (Hash hash) => ByteReprSized (Hash (sign :& hash)) where
  type ByteSize (Hash (sign :& hash)) = ByteSize (Hash hash)

instance (CryptoHash hash) => CryptoHash (sign :& hash) where
  hashBlob          = coerce (hashBlob          @hash)
  hashLazyBlob      = coerce (hashLazyBlob      @hash)
  hashAlgorithmName = coerce (hashAlgorithmName @hash)



----------------------------------------------------------------
-- Key derivation functions
----------------------------------------------------------------

-- | Output of key derivation functions
newtype KDFOutput alg = KDFOutput ByteString

-- | Type class for key derivation functions.
class (ByteReprSized (KDFOutput alg)) => CryptoKDF alg where
  -- | Extra parameters such as nonce, number of iterations etc.
  type KDFParams alg
  -- | Generate random sequence of bytes
  deriveKey :: KDFParams alg -> ByteString -> KDFOutput alg

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
  encryptMessage :: CypherKey alg -> CypherNonce alg -> ByteString -> ByteString
  -- | Decrypt message. If MAC verification fails (message was
  --   tampered with) decription returns @Nothing@
  decryptMessage :: CypherKey alg -> CypherNonce alg -> ByteString -> Maybe ByteString
  -- | Generate random key using cryptographically secure RNG
  generateCypherKey :: MonadIO m => m (CypherKey alg)
  -- | Generate random nonce using cryptographically secure RNG
  generateCypherNonce :: MonadIO m => m (CypherNonce alg)


-- | Value encrypted with stream cypher. It should be created by
--   'makeSecretBox' which takes care of generating nonce.
data SecretBox alg = SecretBox
  { secretBoxValue :: !ByteString
  , secretBoxNonce :: !(CypherNonce alg)
  }
  deriving stock    (Show, Generic)
  deriving anyclass (CBOR.Serialise)

-- | Encrypt value and generate random nonce. This method should be
--   used only if nonce is big enough for collision probability to be
--   negligible.
makeSecretBox :: (MonadIO m, StreamCypher alg) => CypherKey alg -> ByteString -> m (SecretBox alg)
makeSecretBox key msg = do
  nonce <- generateCypherNonce
  return $! SecretBox { secretBoxValue = encryptMessage key nonce msg
                      , secretBoxNonce = nonce
                      }

-- | Decrypt secretbox. It will return @Nothing@ if MAC verification
--   fails. That mean either that message was encrypted.
openSecretBox :: (StreamCypher alg) => CypherKey alg -> SecretBox alg -> Maybe ByteString
openSecretBox key box = decryptMessage key (secretBoxNonce box) (secretBoxValue box)


-- | Value ecrypted using public key cryptography
data PubKeyBox key kdf cypher = PubKeyBox
  { pubKeyBoxValue :: !ByteString
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
  -> ByteString              -- ^ Clear text message
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
  -> Maybe ByteString
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




instance CryptoAsymmetric alg => CryptoHashable (PublicKey alg) where
  hashStep k
    = hashStep (CryPublicKey $ getCryptoName (asymmKeyAlgorithmName @alg))
   <> hashStep (encodeToBS k)

instance CryptoAsymmetric alg => CryptoHashable (PrivKey alg) where
  hashStep k
    = hashStep (CryPrivateKey $ getCryptoName (asymmKeyAlgorithmName @alg))
   <> hashStep (encodeToBS k)

instance CryptoAsymmetric alg => CryptoHashable (Signature alg) where
  hashStep k
    = hashStep (CrySignature $ getCryptoName (asymmKeyAlgorithmName @alg))
   <> hashStep (encodeToBS k)

