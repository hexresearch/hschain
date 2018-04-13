{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TypeFamilies   #-}
-- |
-- Simple API for cryptographic operations. Crypto algorithms are
-- selected by type and all necessary types are implemented as data
-- families
module Thundermint.Crypto (
    -- * Crypto API
    PrivKey
  , PublicKey
  , Signature
  , Address
  , Crypto(..)
    -- * Serialization and signatures
  , Serializable(..)
  , SignedState(..)
  , Signed
  , signedValue
  , signedAddr
  , signValue
  , verifySignature
  ) where

import Control.Monad
-- import qualified Data.ByteString as BS
import           Data.ByteString   (ByteString)
import qualified Data.Map        as Map
import           Data.Map          (Map)
import qualified Data.Set        as Set
import           Data.Set          (Set)

----------------------------------------------------------------
-- Basic crypto API
----------------------------------------------------------------

-- | Private key
data family PrivKey   alg

-- | Public key
data family PublicKey alg

-- | Signature
data family Signature alg

-- |
data family Address   alg

-- |
class Crypto alg where
  signBlob        :: PrivKey alg -> ByteString -> Signature alg
  verifyBlobSignature :: PublicKey alg -> ByteString -> Signature alg -> Bool
  publicKey       :: PrivKey alg -> PublicKey alg
  address         :: PublicKey alg -> Address alg

----------------------------------------------------------------
-- Signing and verification of values
----------------------------------------------------------------

-- | Type class for serialization of values. It exist to sidestep
--   question of encoding choice its stability etc. CBOR seems to be
--   good candidate.
class Serializable a where
  serialize   :: a -> ByteString
  deserialize :: ByteString -> Maybe a


-- | Whether signature has been verified or not
data SignedState = Verified
                 | Unverified

-- | Opaque data type holding
data Signed (sign :: SignedState) alg a
  = Signed (Address alg) (Signature alg) a

signedValue :: Signed sign alg a -> a
signedValue (Signed _ _ a) = a

signedAddr :: Signed sign alg a -> Address alg
signedAddr (Signed a _ _) = a



signValue
  :: (Serializable a, Crypto alg)
  => PrivKey alg
  -> a
  -> Signed 'Verified alg a
signValue privK a
  = Signed (address $ publicKey privK)
           (signBlob privK $ serialize a)
           a

verifySignature
  :: (Serializable a, Crypto alg)
  => (Address alg -> Maybe (PublicKey alg))
  -> Signed 'Unverified alg a
  -> Maybe  (Signed 'Verified alg a)
verifySignature lookupKey (Signed addr signature a) = do
  pubK <- lookupKey addr
  guard $ verifyBlobSignature pubK (serialize a) signature
  return $ Signed addr signature a



----------------------------------------------------------------
-- Collection of signed values
----------------------------------------------------------------

-- | Collection of signed values which is intended for holding votes
--   by validators. We maintain following invariant.
--
--   * Each key could be used to sign only one value
--
--   Lookup both by 'Address' and by values are supported.
data VoteSet ty alg a = VoteSet
  { vsetAddrMap :: Map (Address alg) (Signed ty alg a)
  , vsetValMap  :: Map a (Set (Address alg))
  }

-- | Result of insertion into 'VoteSet'
data InsertRes b a
  = InsertOK       a            -- ^ Insert is successful
  | InsertDup                   -- ^ Duplicate value. No change
  | InsertConflict b            -- ^ Conflict during insertion
  deriving (Show,Functor)

instance Applicative (InsertRes b) where
  pure  = return
  (<*>) = ap

instance Monad (InsertRes b) where
  return = pure
  InsertOK a       >>= f = f a
  InsertDup        >>= _ = InsertDup
  InsertConflict b >>= _ = InsertConflict b

-- | Insert value into set of votes
insertVoteSet
  :: (Crypto alg, Ord (Address alg), Ord a)
  => Signed ty alg a
  -> VoteSet ty alg a
  -> InsertRes (Signed ty alg a) (VoteSet ty alg a)
insertVoteSet sval (VoteSet mAddr mVal) =
  case addr `Map.lookup` mAddr of
    Just v
      | signedValue v == val -> InsertDup
      | otherwise            -> InsertConflict sval
    Nothing                  -> InsertOK VoteSet
      { vsetAddrMap = Map.insert addr sval mAddr
      , vsetValMap  = Map.alter
          (Just . \case
              Nothing        -> Set.singleton addr
              Just addresses -> addr `Set.insert` addresses
          ) val mVal
      }
  where
    addr = signedAddr  sval
    val  = signedValue sval
