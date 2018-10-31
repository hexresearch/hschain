{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
-- |
module Thundermint.Store.Internal.Types where

import Codec.Serialise (Serialise,serialise,deserialiseOrFail)
import Data.Int
import qualified Database.SQLite.Simple           as SQL
import qualified Database.SQLite.Simple.ToField   as SQL
import qualified Database.SQLite.Simple.FromField as SQL


----------------------------------------------------------------
-- Encoding for field of table
----------------------------------------------------------------

-- | Encoding of haskell value as field in SQLite database
data FieldEncoding a where
  FieldEncoding :: (SQL.FromField x, SQL.ToField x)
                => !(a -> x)
                -> !(x -> a)
                -> FieldEncoding a

-- | Value is encoded using CBOR and stored in database
encodingCBOR :: Serialise a => FieldEncoding a
encodingCBOR = FieldEncoding serialise $ \bs -> case deserialiseOrFail bs of
  Right a -> a
  Left  e -> error ("CBOR encoding error: " ++ show e)

encodeField :: FieldEncoding a -> a -> SQL.SQLData
encodeField (FieldEncoding to _) a
  = SQL.toField (to a)


----------------------------------------------------------------
-- Other internal types
----------------------------------------------------------------

-- | Version of persistent structure
data Version
  = New
  | Commited !Int64
  deriving (Show,Eq,Ord)

bumpVersion :: Version -> Version
bumpVersion = \case
  New        -> Commited 1
  Commited n -> Commited (n+1)


-- | Tag for single updates in the database.
data Row
  = RowInsert                   -- ^ Insertion
  | RowDrop                     -- ^ Deletion
  deriving (Show)

instance SQL.ToField Row where
  toField RowInsert = SQL.toField (0 :: Int64)
  toField RowDrop   = SQL.toField (1 :: Int64)

instance SQL.FromField Row where
  fromField f = do
    r <- SQL.fromField f
    case r :: Int64 of
      0 -> return RowInsert
      1 -> return RowDrop
      _ -> fail "Cannot decode Row type"


data UpdateCheck
  = UpdateOK
  | UpdateNoop
  | UpdateBad
