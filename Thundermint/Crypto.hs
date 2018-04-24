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
  ) where

import Codec.Serialise (Serialise,serialise)
import Control.Monad
-- import qualified Data.ByteString as BS
import           Data.Word
import           Data.ByteString.Lazy   (ByteString)
import GHC.Generics (Generic)


----------------------------------------------------------------
-- Basic crypto API
----------------------------------------------------------------

-- | Private key
data family PrivKey   alg

-- | Public key
data family PublicKey alg

-- | Signature
newtype Signature alg = Signature ByteString
  deriving (Show,Eq,Ord, Serialise)

-- |
newtype Address alg = Address ByteString
  deriving (Show,Eq,Ord, Serialise)

-- |
newtype Hash alg = Hash ByteString
  deriving (Show,Eq,Ord, Serialise)

-- | Type-indexed set of crypto algorithms. It's not very principled
--   by to keep signatures sane everything was thrown into same type
--   class.
class Crypto alg where
  signBlob            :: PrivKey   alg -> ByteString -> Signature alg
  verifyBlobSignature :: PublicKey alg -> ByteString -> Signature alg -> Bool
  publicKey           :: PrivKey   alg -> PublicKey alg
  address             :: PublicKey alg -> Address alg
  hashBlob            :: ByteString -> Hash alg


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
           (signBlob privK $ serialise a)
           a

verifySignature
  :: (Serialise a, Crypto alg)
  => (Address alg -> Maybe (PublicKey alg))
  -> Signed 'Unverified alg a
  -> Maybe  (Signed 'Verified alg a)
verifySignature lookupKey (Signed addr signature a) = do
  pubK <- lookupKey addr
  guard $ verifyBlobSignature pubK (serialise a) signature
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
blockHash a = BlockHash 0xFFFFFFFF (hashBlob (serialise a)) []

instance Serialise (BlockHash alg a)
