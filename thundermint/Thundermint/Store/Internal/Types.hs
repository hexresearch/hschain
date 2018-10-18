{-# LANGUAGE LambdaCase #-}
-- |
module Thundermint.Store.Internal.Types where

import Data.Int
import qualified Database.SQLite.Simple           as SQL
import qualified Database.SQLite.Simple.ToField   as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import Lens.Micro


-- | Version of persistent structure
data Version
  = New
  | Commited !Int64
  deriving (Show,Eq,Ord)

bumpVersion :: Version -> Version
bumpVersion = \case
  New        -> Commited 0
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
