{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Simple API for cryptographic operations. Crypto algorithms are
-- selected by type and all necessary types are implemented as data
-- families
module Thundermint.Crypto (
    -- * Crypto API
    PrivKey
  , PublicKey
  , Signature(..) -- FIXME: expose constructoion only
  , Address(..)   -- FIXME: same
  , Hash(..)
  , Crypto(..)
    -- * Serialization and signatures
  , SignedState(..)
  , Signed
  , signedValue
  , signedAddr
  , signValue
  , verifySignature
  , unverifySignature
    -- * Hash trees
  , Hashed(..)
  , BlockHash(..)
  , blockHash
  -- , HashTree(..)
    -- * base58 encoding
  , encodeBase58
  , decodeBase58
  ) where

import Codec.Serialise (Serialise, serialise)
import Control.Monad

import Data.ByteString.Lazy (toStrict)
import Data.Word
import GHC.Generics         (Generic)

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
  deriving (Eq, Ord, Serialise)

instance Show (Signature alg) where
  showsPrec n (Signature bs)
    = showParen (n > 10)
    $ showString "Signature " . shows (encodeBase58 bs)

-- |
newtype Address alg = Address BS.ByteString
  deriving (Eq,Ord, Serialise)

instance Show (Address alg) where
  showsPrec n (Address bs)
    = showParen (n > 10)
    $ showString "Address " . shows (encodeBase58 bs)

-- |
newtype Hash alg = Hash BS.ByteString
  deriving (Eq,Ord, Serialise)

instance Show (Hash alg) where
  showsPrec n (Hash bs)
    = showParen (n > 10)
    $ showString "Hash " . shows (encodeBase58 bs)

-- | Type-indexed set of crypto algorithms. It's not very principled
--   by to keep signatures sane everything was thrown into same type
--   class.
class Crypto alg where
  signBlob            :: PrivKey   alg -> BS.ByteString -> Signature alg
  verifyBlobSignature :: PublicKey alg -> BS.ByteString -> Signature alg -> Bool
  publicKey           :: PrivKey   alg -> PublicKey alg
  address             :: PublicKey alg -> Address alg
  hashBlob            :: BS.ByteString -> Hash alg


----------------------------------------------------------------
-- Signing and verification of values
----------------------------------------------------------------

-- | Whether signature has been verified or not
data SignedState = Verified
                 | Unverified

-- | Opaque data type holding
data Signed (sign :: SignedState) alg a
  = Signed (Address alg) (Signature alg) a
  deriving (Generic)

deriving instance (Show a, Show (Address alg), Show (Signature alg)) => Show (Signed sign alg a)

instance Serialise a => Serialise (Signed 'Unverified alg a)
-- FIXME: we should be able to straight up decode withi\out verifying
--        signature.
instance Serialise a => Serialise (Signed 'Verified alg a)

signedValue :: Signed sign alg a -> a
signedValue (Signed _ _ a) = a

signedAddr :: Signed sign alg a -> Address alg
signedAddr (Signed a _ _) = a



signValue
  :: (Serialise a, Crypto alg)
  => PrivKey alg
  -> a
  -> Signed 'Verified alg a
signValue privK a
  = Signed (address $ publicKey privK)
           (signBlob privK $ toStrict $ serialise a)
           a

verifySignature
  :: (Serialise a, Crypto alg)
  => (Address alg -> Maybe (PublicKey alg))
  -> Signed 'Unverified alg a
  -> Maybe  (Signed 'Verified alg a)
verifySignature lookupKey (Signed addr signature a) = do
  pubK <- lookupKey addr
  guard $ verifyBlobSignature pubK (toStrict $ serialise a) signature
  return $ Signed addr signature a

unverifySignature :: Signed ty alg a -> Signed 'Unverified alg a
unverifySignature (Signed addr sig a) = Signed addr sig a

----------------------------------------------------------------
-- Hashed data
----------------------------------------------------------------

newtype Hashed alg a = Hashed (Hash alg)
  deriving (Show,Eq,Ord, Serialise)

data BlockHash alg a = BlockHash Word32 (Hash alg) [Hash alg]
  deriving (Show,Eq,Ord,Generic)

blockHash
  :: (Crypto alg, Serialise a)
  => a
  -> BlockHash alg a
blockHash a = BlockHash 0xFFFFFFFF (hashBlob (toStrict $ serialise a)) []

instance Serialise (BlockHash alg a)

----------------------------------------------------------------
-- Base58 encoding helpers
----------------------------------------------------------------

encodeBase58 :: BS.ByteString -> BS.ByteString
encodeBase58 = Base58.encodeBase58 Base58.bitcoinAlphabet

decodeBase58 :: BS.ByteString -> Maybe BS.ByteString
decodeBase58 = Base58.decodeBase58 Base58.bitcoinAlphabet
