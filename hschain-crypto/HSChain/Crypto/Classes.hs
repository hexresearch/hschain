{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
module HSChain.Crypto.Classes (
    -- * Conversion to or from bytestrings
    ByteRepr(..)
  , ByteReprSized(..)
    -- * Convenience encoding functions
  , encodeBase58
  , decodeBase58
    -- * Default implementation of type classes' methods
  , ViaBase58(..)
  , ByteReprCBOR(..)
  , defaultShow
  , defaultReadPrec
  , defaultToJSON
  , defaultParseJSON
  , defaultCborEncode
  , defaultCborDecode
    -- * Helpers 
  , encodeBSBase58
  , decodeBSBase58
  , readPrecBSBase58
  ) where

import Codec.Serialise.Decoding (Decoder)
import Codec.Serialise.Encoding (Encoding)
import Control.Applicative
import Control.Monad
import Data.Char     (isAscii)
import Data.Proxy
import qualified Codec.Serialise      as CBOR
import qualified Data.Aeson           as JSON
import Data.Aeson.Types (Parser,toJSONKeyText)
import           Data.Text             (Text)
import qualified Data.Text.Encoding   as T
import Text.Read
import GHC.TypeLits

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BC8
import qualified Data.ByteString.Base58 as Base58

----------------------------------------------------------------
-- Type class
----------------------------------------------------------------

-- | Type class for values which could be represented as
--   bytestring. Not it assumes that it could be converted to
--   bytestring but not necessary could be converted back.
class ByteRepr a where
  decodeFromBS :: BS.ByteString -> Maybe a
  encodeToBS   :: a -> BS.ByteString

instance ByteRepr BS.ByteString where
  decodeFromBS = Just
  encodeToBS   = id

-- | Values which have fixed-size represenations
class (KnownNat (ByteSize a), ByteRepr a) => ByteReprSized a where
  type ByteSize a :: Nat

-- | Encode value as base-58 encoded string
encodeBase58 :: ByteRepr a => a -> Text
encodeBase58 = T.decodeUtf8 . encodeBSBase58 . encodeToBS

-- | Decode value from base-58 encoded string
decodeBase58 :: ByteRepr a => Text -> Maybe a
decodeBase58 = decodeFromBS <=< decodeBSBase58 . T.encodeUtf8

----------------------------------------------------------------
-- Default implementations
----------------------------------------------------------------

-- | Newtype wrapper for deriving instances based on base-58
--   encoding. It implements 'Read', 'Show', and aeson's type classes in
--   term of base-58 encoding.
newtype ViaBase58 (s :: Symbol) a = ViaBase58 a
  deriving newtype ByteRepr

instance ByteRepr a => JSON.ToJSON (ViaBase58 s a) where
  toJSON = defaultToJSON

instance (KnownSymbol s, ByteRepr a) => JSON.FromJSON (ViaBase58 s a) where
  parseJSON = defaultParseJSON (symbolVal (Proxy @s))

instance (ByteRepr a) => JSON.ToJSONKey (ViaBase58 s a) where
  toJSONKey = toJSONKeyText encodeBase58

instance (ByteRepr a, KnownSymbol s) => JSON.FromJSONKey (ViaBase58 s a) where
  fromJSONKey = JSON.FromJSONKeyTextParser $ \s -> case decodeBase58 s of
    Just k  -> return k
    Nothing -> fail ("Incorrect Base58 encoding for " <> symbolVal (Proxy @s))

instance (ByteRepr a) => Show (ViaBase58 s a) where
  show = defaultShow
instance (ByteRepr a) => Read (ViaBase58 s a) where
  readPrec = defaultReadPrec


newtype ByteReprCBOR (s :: Symbol) a = ByteReprCBOR a
  deriving newtype ByteRepr

instance (ByteRepr a, KnownSymbol s) => CBOR.Serialise (ByteReprCBOR s a) where
  encode = defaultCborEncode
  decode = defaultCborDecode (symbolVal (Proxy @s))



-- | Default implementation of 'show' from 'Show'. Value will
--   displayed as string. It's compatible with 'defaultReadPrec'
defaultShow :: ByteRepr a => a -> String
defaultShow = show . BC8.unpack . encodeBSBase58 . encodeToBS

-- | Default implementation of 'readPrec' from 'Read' type class. It's
--   compatible with 'defaultShow'
defaultReadPrec :: ByteRepr a => ReadPrec a
defaultReadPrec = do bs <- readPrecBSBase58
                     case decodeFromBS bs of
                       Just k  -> return k
                       Nothing -> empty

-- | Default implementation of 'JSON.toJSON' method of 'JSON.ToJSON'
--   type class.
defaultToJSON :: ByteRepr a => a -> JSON.Value
defaultToJSON = JSON.String . encodeBase58

-- | Default implementation of 'JSON.parseJSON' method of 'JSON.FromJSON'
--   type class.
defaultParseJSON
  :: ByteRepr a
  => String                     -- ^ Name of data type (used in error messages)
  -> JSON.Value
  -> Parser a
defaultParseJSON name (JSON.String s) =
  case decodeBase58 s of
    Nothing -> fail ("Incorrect Base58 encoding for " <> name)
    Just a  -> return a
defaultParseJSON name _ = fail ("Expecting string for " <> name)


-- | Default implementation of 'CBOR.encode' from 'CBOR.Serialise'
defaultCborEncode :: ByteRepr a => a -> Encoding
defaultCborEncode = CBOR.encode . encodeToBS

defaultCborDecode
  :: ByteRepr a
  => String                     -- ^ Name of data type. Used in error messages
  -> Decoder s a
defaultCborDecode name = do
  bs <- CBOR.decode
  case decodeFromBS bs of
    Nothing -> fail ("Cannot decode " ++ name)
    Just k  -> return k

----------------------------------------------------------------
-- Helpers
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
