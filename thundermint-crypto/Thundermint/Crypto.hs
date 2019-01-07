{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE TemplateHaskell            #-}
-- |
-- Simple API for cryptographic operations. Crypto algorithms are
-- selected by type and all necessary types are implemented as data
-- families
module Thundermint.Crypto (
    -- * Crypto API
    PrivKey
  , PublicKey
  , Signature(..)
  , Address(..)
  , Hash(..)
  , hash
  , Crypto(..)
    -- * Encoding and decoding of values
  , ByteRepr(..)
  , encodeBase58
  , decodeBase58
    -- * Serialization and signatures
  , SignedState(..)
  , Signed
  , signedValue
  , signedAddr
  , signValue
  , verifySignature
  , unverifySignature
  , verifyCborSignature
    -- * Hash trees
  , Hashed(..)
  , BlockHash(..)
  , blockHash
  -- , HashTree(..)
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

import qualified Data.Aeson           as JSON
import           Data.Text              (Text)
import qualified Data.Text.Encoding   as T
import           Data.ByteString.Lazy    (toStrict)
import qualified Data.ByteString.Char8 as BC8
import Data.Char  (isAscii)
import Data.Word
import Text.Read
import Text.ParserCombinators.ReadP
import GHC.Generics         (Generic,Generic1)

import Data.SafeCopy

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base58 as Base58

----------------------------------------------------------------
-- Basic crypto API
----------------------------------------------------------------

-- | Private key
data family PrivKey   alg

-- | Public key
data family PublicKey alg

-- | Signature
newtype Signature alg = Signature BS.ByteString
  deriving (Eq, Ord, Generic, Generic1, Serialise, NFData)

instance  SafeCopy (Signature alg)

-- | Address of public key fingerprint (hash of public key)
newtype Address alg = Address BS.ByteString
  deriving (Eq,Ord, Serialise, Generic)

instance  SafeCopy (Address alg)

-- | Cryptographic hash of some value
newtype Hash alg = Hash BS.ByteString
  deriving (Eq,Ord, Serialise, Generic, NFData)

instance  SafeCopy (Hash alg)

-- | Compute hash of value. It's first serialized using CBOR and then
--   hash of encoded data is computed,
hash :: (Crypto alg, Serialise a) => a -> Hash alg
hash = hashBlob . toStrict . serialise


-- | Type-indexed set of crypto algorithms. It's not very principled
--   by to keep signatures sane everything was thrown into same type
--   class.
class Crypto alg where
  -- | Sign sequence of bytes
  signBlob            :: PrivKey   alg -> BS.ByteString -> Signature alg
  -- | Check that signature is correct
  verifyBlobSignature :: PublicKey alg -> BS.ByteString -> Signature alg -> Bool
  -- | Compute public key from  private key
  publicKey           :: PrivKey   alg -> PublicKey alg
  -- | Compute address or public key fingerprint
  address             :: PublicKey alg -> Address alg
  -- | Compute hash of sequence of bytes
  hashBlob            :: BS.ByteString -> Hash alg
  -- | Create private key from bytestring
  privKeyFromBS       :: BS.ByteString -> Maybe (PrivKey alg)
  -- | Create public key from bytestring
  pubKeyFromBS        :: BS.ByteString -> Maybe (PublicKey alg)
  -- | Convert private key to bytestring
  privKeyToBS         :: PrivKey alg -> BS.ByteString
  -- | Convert public key to bytestring
  pubKeyToBS          :: PublicKey alg -> BS.ByteString


-- | Value could be represented as bytestring.
class ByteRepr a where
  decodeFromBS :: BS.ByteString -> Maybe a
  encodeToBS   :: a -> BS.ByteString

instance Crypto alg => ByteRepr (Hash alg) where
  decodeFromBS            = Just . Hash
  encodeToBS (Hash bs) = bs

instance Crypto alg => ByteRepr (Address alg) where
  decodeFromBS            = Just . Address
  encodeToBS (Address bs) = bs

instance Crypto alg => ByteRepr (Signature alg) where
  decodeFromBS            = Just . Signature
  encodeToBS (Signature bs) = bs

instance Crypto alg => ByteRepr (PublicKey alg) where
  decodeFromBS = pubKeyFromBS
  encodeToBS   = pubKeyToBS

instance Crypto alg => ByteRepr (PrivKey alg) where
  decodeFromBS = privKeyFromBS
  encodeToBS   = privKeyToBS

-- | Encode value as base-58 encoded string
encodeBase58 :: ByteRepr a => a -> Text
encodeBase58 = T.decodeUtf8 . encodeBSBase58 . encodeToBS

-- | Decode value from base-58 encoded string
decodeBase58 :: ByteRepr a => Text -> Maybe a
decodeBase58 = decodeFromBS <=< decodeBSBase58 . T.encodeUtf8


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance Crypto alg => Show (PrivKey alg) where
  show = show . BC8.unpack . encodeBSBase58 . privKeyToBS

instance Crypto alg => Read (PrivKey alg) where
  readPrec = do bs <- readPrecBSBase58
                case privKeyFromBS bs of
                  Just k  -> return k
                  Nothing -> empty


instance Crypto alg => Serialise (PublicKey alg) where
  encode = CBOR.encode . pubKeyToBS
  decode = do bs <- CBOR.decode
              case pubKeyFromBS bs of
                Nothing -> fail "Cannot decode private key"
                Just k  -> return k

instance Crypto alg => SafeCopy (PublicKey alg) where
  putCopy = contain . safePut . pubKeyToBS
  getCopy = contain $ 
    do bs <- safeGet
       case pubKeyFromBS bs of
         Nothing -> fail "Cannot decode private key"
         Just k  -> return k

instance Crypto alg => JSON.ToJSON (PrivKey alg) where
  toJSON = JSON.String . T.decodeUtf8 . encodeBSBase58 . privKeyToBS

instance Crypto alg => JSON.FromJSON (PrivKey alg) where
  parseJSON (JSON.String s) =
    case decodeBSBase58 $ T.encodeUtf8 s of
      Nothing -> fail  "Incorrect Base58 encoding for PrivKey"
      Just bs -> case privKeyFromBS bs of
        Nothing -> fail "Incorrect bytestring representation of PrivKey"
        Just k  -> return k
  parseJSON _          = fail "Expecting PrivKey as string"


----------------------------------------

instance Crypto alg => Show (PublicKey alg) where
  show = show . BC8.unpack . encodeBSBase58 . pubKeyToBS

instance Crypto alg => Read (PublicKey alg) where
  readPrec = do bs <- readPrecBSBase58
                case pubKeyFromBS bs of
                  Just k  -> return k
                  Nothing -> empty

instance Crypto alg => Serialise (PrivKey alg) where
  encode = CBOR.encode . privKeyToBS
  decode = do bs <- CBOR.decode
              case privKeyFromBS bs of
                Nothing -> fail "Cannot decode private key"
                Just k  -> return k

instance Crypto alg => JSON.ToJSON (PublicKey alg) where
  toJSON = JSON.String . T.decodeUtf8 . encodeBSBase58 . pubKeyToBS

instance Crypto alg => JSON.FromJSON (PublicKey alg) where
  parseJSON (JSON.String s) =
    case decodeBSBase58 $ T.encodeUtf8 s of
      Nothing -> fail  "Incorrect Base58 encoding for PrivKey"
      Just bs -> case pubKeyFromBS bs of
        Nothing -> fail "Incorrect bytestring representation of PrivKey"
        Just k  -> return k
  parseJSON _          = fail "Expecting PrivKey as string"


----------------------------------------


instance Show (Address alg) where
  showsPrec n (Address bs)
    = showParen (n > 10)
    $ showString "Address " . shows (encodeBSBase58 bs)

instance Read (Address alg) where
  readPrec = do void $ lift $ string "Address" >> some (char ' ')
                Address <$> readPrecBSBase58

instance JSON.ToJSON (Address alg) where
  toJSON (Address s) = JSON.String $ T.decodeUtf8 $ encodeBSBase58 s

instance JSON.FromJSON (Address alg) where
  parseJSON (JSON.String s) =
    case decodeBSBase58 $ T.encodeUtf8 s of
      Nothing -> fail  "Incorrect Base58 encoding while decoding Address"
      Just bs -> return $ Address bs
  parseJSON _ = fail "Expected string for Address"


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
      Nothing -> fail  "Incorrect Base58 encoding while decoding Address"
      Just bs -> return $ Signature bs
  parseJSON _ = fail "Expected string for Signature"



----------------------------------------

instance Show (Hash alg) where
  showsPrec n (Hash bs)
    = showParen (n > 10)
    $ showString "Hash " . shows (encodeBSBase58 bs)

instance Read (Hash alg) where
  readPrec = do void $ lift $ string "Hash" >> some (char ' ')
                Hash <$> readPrecBSBase58

instance JSON.ToJSON (Hash alg) where
  toJSON (Hash s) = JSON.String $ T.decodeUtf8 $ encodeBSBase58 s

instance JSON.FromJSON (Hash alg) where
  parseJSON (JSON.String s) =
    case decodeBSBase58 $ T.encodeUtf8 s of
      Nothing -> fail  "Incorrect Base58 encoding for bs"
      Just bs -> return $ Hash bs
  parseJSON _ = fail "Expected string for Hash"



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
  = Signed !(Address alg) !(Signature alg) !a
  deriving (Generic, Eq, Show)

-- | Obtain underlying value
signedValue :: Signed sign alg a -> a
signedValue (Signed _ _ a) = a

-- | Obtain address used for signing
signedAddr :: Signed sign alg a -> Address alg
signedAddr (Signed a _ _) = a


-- | Sign value. Not that we can generate both verified and unverified
--   values this way.
signValue
  :: (Serialise a, Crypto alg)
  => PrivKey alg                -- ^ Key for signing
  -> a                          -- ^ Value to sign
  -> Signed sign alg a
signValue privK a
  = Signed (address $ publicKey privK)
           (signBlob privK $ toStrict $ serialise a)
           a

-- | Verify signature. It return Nothing if verification fails for any
--   reason. Note that since @Signed@ contain only fingerprint we need
--   to supply function for looking up public keys.
verifySignature
  :: (Serialise a, Crypto alg)
  => (Address alg -> Maybe (PublicKey alg))
     -- ^ Lookup function for public keys. If address is unknown (this
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
  :: (Serialise a, Crypto alg)
  => PublicKey alg
  -> a
  -> Signature alg
  -> Bool
verifyCborSignature pk a
  = verifyBlobSignature pk (toStrict $ serialise a)

instance SafeCopy a => SafeCopy (Signed sign alg a)
instance Serialise a => Serialise (Signed 'Unverified alg a)
instance JSON.FromJSON a => JSON.FromJSON (Signed 'Unverified alg a)
instance JSON.ToJSON   a => JSON.ToJSON   (Signed 'Unverified alg a)

-- FIXME: we should be able to straight up decode withi\out verifying
--        signature.
instance Serialise a => Serialise (Signed 'Verified alg a)
instance JSON.FromJSON a => JSON.FromJSON (Signed 'Verified alg a)
instance JSON.ToJSON   a => JSON.ToJSON   (Signed 'Verified alg a)


----------------------------------------------------------------
-- Hashed data
----------------------------------------------------------------

newtype Hashed alg a = Hashed (Hash alg)
  deriving (Show,Eq,Ord, Serialise)

data BlockHash alg a = BlockHash Word32 (Hash alg) [Hash alg]
  deriving (Show,Eq,Ord,Generic)

instance SafeCopy (BlockHash alg a) 

blockHash
  :: (Crypto alg, Serialise a)
  => a
  -> BlockHash alg a
blockHash a = BlockHash 0xFFFFFFFF (hashBlob (toStrict $ serialise a)) []

instance Serialise     (BlockHash alg a)
instance JSON.ToJSON   (BlockHash alg a)
instance JSON.FromJSON (BlockHash alg a)


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
