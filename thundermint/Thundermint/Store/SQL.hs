{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Data types for interaction with database
module Thundermint.Store.SQL (
    -- * DB connection
    Connection
  , openConnection
  , closeConnection
  , withConnection
    -- ** Encoding for single field in table
  , FieldEncoding(..)
  , encodingCBOR
    -- * SQL wrappers
  , Access(..)
  , Query
  , QueryRO
  , toQueryRO
  , RunQueryRO(..)
  , RunQueryRW(..)
    -- * Persistent data
  , PersistentData(..)
  , Persistent(..)
    -- ** Concrete API
  , PMap(..)
  , ExecutorRO(..)
  , ExecutorRW(..)
    -- ** Implementations
  , EffectfulQ
  , runBlockUpdate
  , EphemeralQ
  , runEphemeralQ
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Text (Text)
import Data.Functor.Compose
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict as Map
import           Database.SQLite.Simple   (Only(..))
import Lens.Micro
import Lens.Micro.Mtl

import Thundermint.Blockchain.Types
import Thundermint.Control
import Thundermint.Store.Internal.Types
import Thundermint.Store.Internal.Query



----------------------------------------------------------------
-- Execution of state update
----------------------------------------------------------------

-- | Type class for data structures which are persisted to database.
class PersistentData a where
  -- | Names of underlying tables
  tableName   :: a -> Text
  -- | Create underlying tables if they do not exist.
  createTable :: a -> Query 'RW ()

-- | Wrapper for carrying class dictionary with data type
data Persistent a where
  PersistentPMap :: PMap k v -> Persistent (PMap k v)

instance PersistentData (Persistent a) where
  tableName = \case
    PersistentPMap p -> tableName p
  createTable = \case
    PersistentPMap p -> createTable p


-- | Implementation of specific operations for persistent data
--   structures.
class (Monad q) => ExecutorRO q where
  type Dct q :: (* -> *) -> *
  lookupKey  :: (Ord k)
             => (forall f. Lens' (Dct q f) (f (PMap k v))) -> k -> q (Maybe v)

-- | Read-write operations 
class (ExecutorRO q) => ExecutorRW q where
  storeKey :: (Ord k, Eq v)
           => (forall f. Lens' (Dct q f) (f (PMap k v))) -> k -> v -> q ()
  dropKey  :: (Ord k, Eq v)
           => (forall f. Lens' (Dct q f) (f (PMap k v))) -> k -> q ()





-- | Map which is persisted on disk
data PMap k v = PMap
  { pmapTableName :: Text       -- Name of underlying SQL table
  , pmapEncodingK :: FieldEncoding k
  , pmapEncodingV :: FieldEncoding v
  }

instance PersistentData (PMap k v) where
  tableName = pmapTableName
  createTable PMap{pmapTableName=tbl} = execute_ $
    "CREATE TABLE IF NOT EXISTS "<>tbl<>
    "( ver INTEGER PRIMARY KEY, \
    \  tag INTEGER, \
    \  key, \
    \  val, \
    \  FOREIGN KEY verDrop REFERENCES vec)"
    -- NOTE: ver field is versions PMap. Each change to PMap is
    --       represented by row and numbered consecutively
    --
    --       tag if flag telling whether row is insertion or
    --       deletion



----------------------------------------------------------------
-- Effectful queries
----------------------------------------------------------------

-- | Monad for execution of queries which actually change state of
--   database. Note that it's possible to replay transaction over
--   already existing data in which case thy will be accepted iff they
--   would result in same changes as recorded.
newtype EffectfulQ (rw :: Access) dct a = EffectfulQ
  { unEffectfulQ :: StateT (dct Versioned) (Query rw) a }
  deriving (Functor, Applicative, Monad)

data Versioned a = Versioned !Version !(Persistent a)

versionedV :: Lens' (Versioned a) Version
versionedV = lens (\(Versioned v _) -> v) (\(Versioned _ d) v -> Versioned v d) 


-- | Update user's state for single block. If we trying to execute
--   already commited changes transaction will only succeed if it will
--   produce exactly same results as already commited.
runBlockUpdate
  :: (FloatOut dct)
  => Height                -- ^ Height of commited block
  -> dct Persistent        -- ^ Description of user state
  -> EffectfulQ 'RW dct a  -- ^ Action to execute
  -> Query 'RW a
runBlockUpdate h dct (EffectfulQ effect) = do
  -- 1. Find out version for state at given height.
  ver <- floatOut $ fmapF (Compose . versionForHeight h) dct
  -- 2. Execute query.
  (a,ver') <- runStateT effect ver 
  -- 3. Write down new checkpoint
  traverseEff (storeCheckpoint h) ver'
  return a


instance ExecutorRO (EffectfulQ rw dct) where
  type Dct (EffectfulQ rw dct) = dct
  lookupKey getter k = EffectfulQ $ do
    Versioned ver (PersistentPMap pmap) <- use getter
    lift $ lookupKeyPMap pmap ver k

instance (rw ~ 'RW) => ExecutorRW (EffectfulQ rw dct) where
  storeKey getter k v = EffectfulQ $ do
    Versioned ver (PersistentPMap pmap@PMap{pmapTableName=tbl, ..}) <- use getter
    lift (checkPMapInsert pmap ver k v) >>= \case
      UpdateOK   -> lift $ execute
        ("INSERT INTO "<>tbl<>" VALUES (NULL,?,?,?)")
        ( RowInsert
        , encodeField pmapEncodingK k
        , encodeField pmapEncodingV v
        )
      UpdateNoop -> return ()
      UpdateBad  -> rollback
    getter . versionedV %= bumpVersion
  --
  dropKey getter k = EffectfulQ $ do
    Versioned ver (PersistentPMap pmap@PMap{pmapTableName=tbl, ..}) <- use getter
    lift (checkPMapDrop pmap ver k) >>= \case
      UpdateOK -> lift $ execute
        ("INSERT INTO "<>tbl<>" VALUES (NULL,?,?,NULL)")
        ( RowDrop
        , encodeField pmapEncodingK k
        )
      UpdateNoop -> return ()
      UpdateBad  -> rollback
    getter . versionedV %= bumpVersion


--
lookupKeyPMap :: PMap k v -> Version -> k -> Query rw (Maybe v)
lookupKeyPMap _ New _ = return Nothing
lookupKeyPMap PMap{pmapTableName=tbl, ..} (Commited ver) k = do
  case pmapEncodingV of
    FieldEncoding _ from -> do
      r <- query
        ("SELECT val, tag FROM "<>tbl<>" WHERE key=? AND ver <= ? ORDER BY ver")
        (encodeField pmapEncodingK k, ver)
      case r of
        []                            -> return Nothing
        [(v, RowInsert)]              -> return $ Just $ from v
        [(_, RowInsert),(_, RowDrop)] -> return Nothing
        _                             -> error "PMap: inconsistent DB"

checkPMapInsert :: (Eq v) => PMap k v -> Version -> k -> v -> Query rw UpdateCheck
checkPMapInsert pmap version k v = do
  tblHead <- tableHead pmap
  case tblHead `compare` version of
    LT -> error "We trying to update from point that not reached"
    -- We updating HEAD and will make real write
    EQ -> checkPMapInsertReal pmap k
    -- We doing replay. We need to check that update will be
    -- identical to recorded
    GT -> checkPMapInsertReplay pmap version k v

checkPMapInsertReal :: PMap k v -> k -> Query rw UpdateCheck
checkPMapInsertReal PMap{pmapTableName=tbl, ..} k = do
  r <- query ("SELECT tag FROM "<>tbl<>" WHERE key = ?")
             (Only $ encodeField pmapEncodingK k)
  case r :: [Only Row] of
    [] -> return UpdateOK
    _  -> return UpdateBad

checkPMapInsertReplay :: (Eq v) => PMap k v -> Version -> k -> v -> Query rw UpdateCheck
checkPMapInsertReplay PMap{pmapTableName=tbl, ..} version k v = do
  case pmapEncodingV of
    FieldEncoding _ from -> do
      r <- query1 ("SELECT tag, val FROM "<>tbl<>" WHERE key = ? AND ver = ?")
                  ( encodeField pmapEncodingK k
                  , case version of New        -> 0
                                    Commited i -> i + 1
                  )
      case r of
        Just (RowInsert, v') | v == from v' -> return UpdateNoop
        _                                   -> return UpdateBad

checkPMapDrop :: PMap k v -> Version -> k -> Query rw UpdateCheck
checkPMapDrop pmap version k = do
  tblHead <- tableHead pmap
  case tblHead `compare` version of
    LT -> error "We trying to update from point that not reached"
    EQ -> checkPMapDropReal pmap k
    GT -> checkPMapDropReplay pmap version k

checkPMapDropReal :: PMap k v -> k -> Query rw UpdateCheck
checkPMapDropReal PMap{pmapTableName=tbl, ..} k = do
  r <- query ("SELECT tag FROM "<>tbl<>" WHERE key = ?")
             (Only $ encodeField pmapEncodingK k)
  case r of
    [Only RowInsert] -> return UpdateOK
    _                -> return UpdateBad

checkPMapDropReplay :: PMap k v -> Version -> k -> Query rw UpdateCheck
checkPMapDropReplay PMap{pmapTableName=tbl, ..} version k = do
  r <- query1 ("SELECT tag FROM "<>tbl<>" WHERE key = ? AND ver = ?")
              ( encodeField pmapEncodingK k
              , case version of New        -> 0
                                Commited i -> i + 1
              )
  case r of
    Just (Only RowDrop) -> return UpdateNoop
    _                   -> return UpdateBad


----------------------------------------------------------------
-- Ephemeral updates
----------------------------------------------------------------

-- | Ephemeral execution of updates to user state. Nothing is recorded
--   to database. Semantics is subtly different. Execution always
--   starts from latest user state and there's support for
--   backtracking implemented using 'Alternative'
newtype EphemeralQ dct a = EphemeralQ
  { unEphemeral :: StateT (dct Overlay) (MaybeT (Query 'RO)) a }
  deriving (Functor, Applicative, Monad, Alternative)

data Overlay a where
  OverlayPMap :: PMap k v -> Map k (Maybe v) -> Overlay (PMap k v)

overlayPMap :: Lens' (Overlay (PMap k v)) (Map k (Maybe v))
overlayPMap = lens (\(OverlayPMap _ o) -> o) (\(OverlayPMap p _) o -> OverlayPMap p o)


-- | Run transaction without changing database. Since it's used for
--   checking transactions or blocks it always uses state
--   corresponding to latest commited block.
runEphemeralQ
  :: (FloatOut dct)
  => dct Persistent    -- ^ Description of user state
  -> EphemeralQ dct a  -- ^ Action to execute
  -> Query 'RO a
runEphemeralQ dct (EphemeralQ effect) = do
  -- 1. Find out version for state at given height.
  let overlay = fmapF makeOverlay dct
  -- 2. Execute query.
  Just a <- runMaybeT $ evalStateT effect overlay
  return a



instance ExecutorRO (EphemeralQ dct) where
  type Dct (EphemeralQ dct) = dct
  lookupKey getter k = EphemeralQ $ do
    OverlayPMap pmap overlay <- use getter
    case k `Map.lookup` overlay of
      Just r  -> return r
      Nothing -> lift $ lift $ lookupKeyPMap pmap (Commited maxBound) k

instance ExecutorRW (EphemeralQ dct) where
  storeKey getter k v = EphemeralQ $ do
    OverlayPMap pmap overlay <- use getter
    -- If we inserted/removed key insert is not valid anyway
    when (k `Map.member` overlay) rollback
    -- Check for insert conflicts in DB
    lift (lift (checkPMapInsertReal pmap k)) >>= \case
      UpdateOK   -> getter . overlayPMap %= Map.insert k (Just v)
      UpdateBad  -> rollback
      UpdateNoop -> error "Impossible"
  --
  dropKey getter k = EphemeralQ $ do
    OverlayPMap pmap overlay <- use getter
    case k `Map.lookup` overlay of
      Just Nothing  -> rollback
      Just (Just _) -> getter . overlayPMap %= Map.insert k Nothing
      Nothing       -> lift (lift (checkPMapDropReal pmap k)) >>= \case
        UpdateOK   -> getter . overlayPMap %= Map.insert k Nothing
        UpdateBad  -> rollback
        UpdateNoop -> error "Impossible"




----------------------------------------------------------------
-- Versioning helpers
----------------------------------------------------------------


tableHead :: PersistentData a => a -> Query rw Version
tableHead a =
  query ("SELECT MAX(id) FROM "<>tbl) () >>= \case
    [Only Nothing ] -> return New
    [Only (Just v)] -> return (Commited v)
    _               -> error "Impossible"
  where
   tbl = tableName a


versionForHeight
  :: Height
  -> Persistent a
  -> Query rw (Versioned a)
versionForHeight (Height 0) p = return $ Versioned New p
versionForHeight (Height h) p = do
  r <- query1 "SELECT id FROM thm_checkpoints WHERE height = ? AND tableName = ?"
              (h-1, tableName p)
  case r of
    Nothing              -> rollback
    Just (Only Nothing)  -> return $ Versioned New p
    Just (Only (Just i)) -> return $ Versioned (Commited i) p
  

storeCheckpoint
  :: Height
  -> Versioned a
  -> Query 'RW ()
storeCheckpoint (Height h) (Versioned ver p) = do
  r <- query1 "SELECT id FROM thm_checkpoints WHERE height = ? AND tableName = ?"
              (h, tableName p)
  case r of
    Nothing       -> execute "INSERT INTO thm_checkpoints VALUES (?,?,?)"
                             (tableName p, h, v)
    Just (Only v')
      | v /= v'   -> rollback
      | otherwise -> rollback
  where
    v = case ver of New        -> Nothing
                    Commited i -> Just i
    
makeOverlay :: Persistent a -> Overlay a
makeOverlay = \case
  PersistentPMap p -> OverlayPMap p Map.empty
