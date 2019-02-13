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
    -- * Persistent data
  , PersistentData(..)
  , Persistent(..)
    -- ** Concrete API
  , ExecutorRO(..)
  , ExecutorRW(..)
    -- *** Data types
  , PMap(..)
  , PSet
  , PSet'
  , SetType(..)
  , PersistentSet(..)
    -- ** Implementations
  , EffectfulQ
  , runBlockUpdate
  , queryUserState
  , EphemeralQ
  , runEphemeralQ
    --
  , persistentTableName
  , persistentCreateTable
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Functor.Compose
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict as Map
import           Data.Set          (Set)
import qualified Data.Set        as Set
import           Database.SQLite.Simple   (Only(..))
import Lens.Micro
import Lens.Micro.Mtl

import Thundermint.Types.Blockchain
import Thundermint.Control
import Thundermint.Store.Internal.Types
import Thundermint.Store.Internal.Query



----------------------------------------------------------------
-- Execution of state update
----------------------------------------------------------------

-- | Type class for data structures which are persisted to database.
class PersistentData t where
  -- | Names of underlying tables
  tableName   :: t -> Text
  -- | Create underlying tables if they do not exist.
  createTable :: t -> Query 'RW alg a ()
  -- |
  wrap        :: t -> Persistent t

-- | Wrapper for carrying class dictionary with data type
data Persistent a where
  PersistentPMap :: !(PMap k v)           -> Persistent (PMap k v)
  PersistentPSet :: !(PersistentSet ty k) -> Persistent (PersistentSet ty k)

-- | Implementation of specific operations for persistent data
--   structures.
class ExecutorRO q where
  lookupKey       :: (Ord k)
                  => (forall f. Lens' (dct f) (f (PMap k v)))
                  -> k -> q dct (Maybe v)
  materializePMap :: (Ord k)
                  => (forall f. Lens' (dct f) (f (PMap k v)))
                  -> q dct (Map k v)

  isMember        :: (Ord k)
                  => (forall f. Lens' (dct f) (f (PersistentSet ty k)))
                  -> k -> q dct Bool
  materializePSet :: (Ord k)
                  => (forall f. Lens' (dct f) (f (PersistentSet ty k)))
                  -> q dct (Set k)

-- | Read-write operations
class (ExecutorRO q) => ExecutorRW q where
  storeKey     :: (Ord k, Eq v)
               => (forall f. Lens' (dct f) (f (PMap k v)))
               -> k -> v -> q dct ()
  dropKey      :: (Ord k, Eq v)
               => (forall f. Lens' (dct f) (f (PMap k v)))
               -> k -> q dct ()
  storeSetElem :: (Ord k)
               => (forall f. Lens' (dct f) (f (PersistentSet ty k)))
               -> k -> q dct ()
  dropSetElem  :: (Ord k)
               => (forall f. Lens' (dct f) (f (PSet k)))
               -> k -> q dct ()



-- | Map which is persisted on disk
data PMap k v = PMap
  { pmapTableName :: !Text       -- Name of underlying SQL table
  , pmapEncodingK :: !(FieldEncoding k)
  , pmapEncodingV :: !(FieldEncoding v)
  }

instance PersistentData (PMap k v) where
  tableName = pmapTableName
  createTable PMap{pmapTableName=tbl} = execute_ $
    "CREATE TABLE IF NOT EXISTS "<>tbl<>
    "( ver INTEGER PRIMARY KEY, \
    \  tag INTEGER, \
    \  key, \
    \  val)"
    -- NOTE: ver field is versions PMap. Each change to PMap is
    --       represented by row and numbered consecutively
    --
    --       tag is flag telling whether row is insertion or
    --       deletion
  wrap = PersistentPMap



-- | Set where elements could be added and removed but after removal
--   attempt to readd element will fail.
type PSet = PersistentSet 'StandardSet

-- | Set where elements could be added but never removed.
type PSet' = PersistentSet 'AppendOnlySet

data SetType = AppendOnlySet -- ^ Elements could be only added to set
             | StandardSet   -- ^ Value could be added and removed

-- | Persistet set. It comes in two varieties: one that allows
--   appending only and one which allows deletion.
data PersistentSet (ty :: SetType) k = PSet
  { psetTableName :: !Text
  , psetEncodingK :: !(FieldEncoding k)
  }

instance PersistentData (PersistentSet ty k) where
  tableName = psetTableName
  createTable PSet{psetTableName=tbl} = execute_ $
    "CREATE TABLE IF NOT EXISTS "<>tbl<>
    "( ver INTEGER PRIMARY KEY, \
    \  tag INTEGER, \
    \  key)"
  wrap = PersistentPSet

----------------------------------------------------------------
-- Effectful queries
----------------------------------------------------------------

-- | Monad for execution of queries which actually change state of
--   database. Note that it's possible to replay transaction over
--   already existing data in which case thy will be accepted iff they
--   would result in same changes as recorded.
newtype EffectfulQ (rw :: Access) alg a dct x = EffectfulQ
  { _unEffectfulQ :: StateT (dct Versioned) (Query rw alg a) x }
  deriving (Functor, Applicative, Monad)

data Versioned a = Versioned !Version !(Persistent a)

versionedV :: Lens' (Versioned a) Version
versionedV = lens (\(Versioned v _) -> v) (\(Versioned _ d) v -> Versioned v d)


-- | Update user's state for single block. If we trying to execute
--   already commited changes transaction will only succeed if it will
--   produce exactly same results as already commited.
runBlockUpdate
  :: (FloatOut dct)
  => Height                      -- ^ Height of commited block
  -> dct Persistent              -- ^ Description of user state
  -> EffectfulQ 'RW alg a dct x  -- ^ Action to execute
  -> Query 'RW alg a x
runBlockUpdate h dct (EffectfulQ effect) = do
  -- 1. Find out version for state at given height.
  ver <- floatOut $ fmapF (Compose . versionForHeight h) dct
  -- 2. Execute query.
  (a,ver') <- runStateT effect ver
  -- 3. Write down new checkpoint
  traverseEff (storeCheckpoint h) ver'
  return a

-- | Update user's state for single block. If we trying to execute
--   already commited changes transaction will only succeed if it will
--   produce exactly same results as already commited.
queryUserState
  :: (FloatOut dct)
  => Height                      -- ^ Height of commited block
  -> dct Persistent              -- ^ Description of user state
  -> EffectfulQ 'RO alg a dct x  -- ^ Action to execute
  -> Query 'RO alg a x
queryUserState h dct (EffectfulQ effect) = do
  ver   <- floatOut $ fmapF (Compose . versionForHeight (succ h)) dct
  (a,_) <- runStateT effect ver
  return a


instance ExecutorRO (EffectfulQ rw alg a) where
  lookupKey getter k = EffectfulQ $ do
    Versioned ver (PersistentPMap pmap) <- use getter
    lift $ lookupKeyPMap pmap ver k
  --
  materializePMap getter = EffectfulQ $ do
    Versioned ver (PersistentPMap pmap) <- use getter
    lift $ materializePMapWorker pmap ver
  --
  isMember getter k = EffectfulQ $ do
    Versioned ver (PersistentPSet pset) <- use getter
    lift $ isMemberPSet pset ver k
  --
  materializePSet getter = EffectfulQ $ do
    Versioned ver (PersistentPSet pset) <- use getter
    lift $ materializePSetWorker pset ver


instance (rw ~ 'RW) => ExecutorRW (EffectfulQ rw alg a) where
  storeKey getter k v = EffectfulQ $ do
    Versioned ver (PersistentPMap pmap@PMap{pmapTableName=tbl, ..}) <- use getter
    lift $ checkPMapInsert pmap ver k v `checkedReal` execute
      ("INSERT INTO "<>tbl<>" VALUES (NULL,?,?,?)")
      ( RowInsert
      , encodeField pmapEncodingK k
      , encodeField pmapEncodingV v
      )
    getter . versionedV %= bumpVersion
  --
  dropKey getter k = EffectfulQ $ do
    Versioned ver (PersistentPMap pmap@PMap{pmapTableName=tbl, ..}) <- use getter
    lift $ checkPMapDrop pmap ver k `checkedReal` execute
      ("INSERT INTO "<>tbl<>" VALUES (NULL,?,?,NULL)")
      ( RowDrop
      , encodeField pmapEncodingK k
      )
    getter . versionedV %= bumpVersion
  --
  storeSetElem getter k = EffectfulQ $ do
    Versioned ver (PersistentPSet pset@PSet{psetTableName=tbl, ..}) <- use getter
    lift $ checkPSetStore pset ver k `checkedReal` execute
      ("INSERT INTO "<>tbl<>" VALUES (NULL,?,?)")
      ( RowInsert, encodeField psetEncodingK k)
    getter . versionedV %= bumpVersion
  --
  dropSetElem getter k = EffectfulQ $ do
    Versioned ver (PersistentPSet pset@PSet{psetTableName=tbl, ..}) <- use getter
    lift $ checkPSetDrop pset ver k `checkedReal` execute
      ("INSERT INTO "<>tbl<>" VALUES (NULL,?,?)")
      ( RowDrop
      , encodeField psetEncodingK k
      )
    getter . versionedV %= bumpVersion

checkedReal :: (Monad m) => m UpdateCheck -> m () -> m ()
checkedReal check action = check >>= \case
  UpdateOK   -> action
  UpdateNoop -> return ()
  UpdateBad  -> fail "ROLLBACK"



lookupKeyPMap :: PMap k v -> Version -> k -> Query rw alg a (Maybe v)
lookupKeyPMap _ New _ = return Nothing
lookupKeyPMap PMap{pmapTableName=tbl, ..} (Commited ver) k = do
  case pmapEncodingV of
    FieldEncoding _ from -> do
      r <- query
        ("SELECT val, tag FROM "<>tbl<>" WHERE key=? AND ver <= ? ORDER BY ver")
        (encodeField pmapEncodingK k, ver)
      case r of
        []                            -> return Nothing
        [(Just v, RowInsert)]         -> return $ Just $ from v
        [(_, RowInsert),(_, RowDrop)] -> return Nothing
        _                             -> error "PMap: inconsistent DB"

materializePMapWorker :: Ord k => PMap k v -> Version -> Query rw alg a (Map k v)
materializePMapWorker _                           New            = return Map.empty
materializePMapWorker PMap{pmapTableName=tbl, ..} (Commited ver) = do
  case (pmapEncodingK, pmapEncodingV) of
    (FieldEncoding _ fromK, FieldEncoding _ fromV) -> do
      r <- query
        ("SELECT key,val FROM "<>tbl<>
         " WHERE tag=? AND ver<=? AND key NOT IN (SELECT key FROM "<>tbl<>" WHERE tag = ? AND ver<=?)")
        (RowInsert, ver, RowDrop, ver)
      return $ Map.fromList [ (fromK k, fromV v) | (k,v) <- r ]

isMemberPSet :: PersistentSet ty k -> Version -> k -> Query rw alg a Bool
isMemberPSet _                           New            _ = return False
isMemberPSet PSet{psetTableName=tbl, ..} (Commited ver) k = do
  r <- query
    ("SELECT tag FROM "<>tbl<>" WHERE key=? AND ver <= ? ORDER BY ver")
    (encodeField psetEncodingK k, ver)
  case r :: [Only Row] of
    []                             -> return False
    [Only RowInsert]               -> return True
    [Only RowInsert, Only RowDrop] -> return False
    _                              -> error "PSet: inconsistent DB"

materializePSetWorker :: Ord k => PersistentSet ty k -> Version -> Query rw alg a (Set k)
materializePSetWorker _                           New = return Set.empty
materializePSetWorker PSet{psetTableName=tbl, ..} (Commited ver) =
  case psetEncodingK of
    FieldEncoding _ from -> do
      r <- query
        ("SELECT key FROM "<>tbl<>
         " WHERE tag=? AND ver<=? AND key NOT IN (SELECT key FROM "<>tbl<>" WHERE tag = ? AND ver<=?)")
        (RowInsert, ver, RowDrop, ver)
      return $ Set.fromList $ from . fromOnly <$> r



checkPMapInsert :: (Eq v) => PMap k v -> Version -> k -> v -> Query rw alg a UpdateCheck
checkPMapInsert pmap version k v = do
  tblHead <- tableHead pmap
  case tblHead `compare` version of
    LT -> error "We trying to update from point that not reached"
    -- We updating HEAD and will make real write
    EQ -> checkPMapInsertReal pmap k
    -- We doing replay. We need to check that update will be
    -- identical to recorded
    GT -> checkPMapInsertReplay pmap version k v

checkPMapInsertReal :: PMap k v -> k -> Query rw alg a UpdateCheck
checkPMapInsertReal PMap{pmapTableName=tbl, ..} k = do
  r <- query ("SELECT tag FROM "<>tbl<>" WHERE key = ?")
             (Only $ encodeField pmapEncodingK k)
  case r :: [Only Row] of
    [] -> return UpdateOK
    _  -> return UpdateBad

checkPMapInsertReplay :: (Eq v) => PMap k v -> Version -> k -> v -> Query rw alg a UpdateCheck
checkPMapInsertReplay PMap{pmapTableName=tbl, ..} version k v = do
  case pmapEncodingV of
    FieldEncoding _ from -> do
      r <- query1 ("SELECT tag, val FROM "<>tbl<>" WHERE key = ? AND ver = ?")
                  ( encodeField pmapEncodingK k
                  , bumpForDB version
                  )
      case r of
        Just (RowInsert, v') | v == from v' -> return UpdateNoop
        _                                   -> return UpdateBad



checkPMapDrop :: PMap k v -> Version -> k -> Query rw alg a UpdateCheck
checkPMapDrop pmap version k = do
  tblHead <- tableHead pmap
  case tblHead `compare` version of
    LT -> error "We trying to update from point that not reached"
    EQ -> checkPMapDropReal pmap k
    GT -> checkPMapDropReplay pmap version k

checkPMapDropReal :: PMap k v -> k -> Query rw alg a UpdateCheck
checkPMapDropReal PMap{pmapTableName=tbl, ..} k = do
  r <- query ("SELECT tag FROM "<>tbl<>" WHERE key = ?")
             (Only $ encodeField pmapEncodingK k)
  case r of
    [Only RowInsert] -> return UpdateOK
    _                -> return UpdateBad

checkPMapDropReplay :: PMap k v -> Version -> k -> Query rw alg a UpdateCheck
checkPMapDropReplay PMap{pmapTableName=tbl, ..} version k = do
  r <- query1 ("SELECT tag FROM "<>tbl<>" WHERE key = ? AND ver = ?")
              ( encodeField pmapEncodingK k
              , bumpForDB version
              )
  case r of
    Just (Only RowDrop) -> return UpdateNoop
    _                   -> return UpdateBad



checkPSetStore :: PersistentSet ty k -> Version -> k -> Query rw alg a UpdateCheck
checkPSetStore pset version k = do
  tblHead <- tableHead pset
  case tblHead `compare` version of
    LT -> error "We trying to update from point that not reached"
    EQ -> checkPSetStoreReal   pset k
    GT -> checkPSetStoreReplay pset version k

checkPSetStoreReal :: PersistentSet ty k -> k -> Query rw alg a UpdateCheck
checkPSetStoreReal PSet{psetTableName=tbl, ..} k = do
  r <- query ("SELECT tag FROM "<>tbl<>" WHERE key = ?")
             (Only $ encodeField psetEncodingK k)
  case r :: [Only Row] of
    [] -> return UpdateOK
    _  -> return UpdateBad

checkPSetStoreReplay :: PersistentSet ty k -> Version -> k -> Query rw alg a UpdateCheck
checkPSetStoreReplay PSet{psetTableName=tbl, ..} version k = do
  r <- query1 ("SELECT tag FROM "<>tbl<>" WHERE key = ? AND ver = ?")
              ( encodeField psetEncodingK k
              , bumpForDB version
              )
  case r of
    Just (Only RowInsert) -> return UpdateNoop
    _                     -> return UpdateBad



checkPSetDrop :: PersistentSet ty k -> Version -> k -> Query rw alg a UpdateCheck
checkPSetDrop pset version k = do
  tblHead <- tableHead pset
  case tblHead `compare` version of
    LT -> error "We trying to update from point that not reached"
    EQ -> checkPSetDropReal   pset k
    GT -> checkPSetDropReplay pset version k

checkPSetDropReal :: PersistentSet ty k -> k -> Query rw alg a UpdateCheck
checkPSetDropReal PSet{psetTableName=tbl, ..} k = do
  r <- query ("SELECT tag FROM "<>tbl<>" WHERE key = ?")
             (Only $ encodeField psetEncodingK k)
  case r of
    [Only RowInsert] -> return UpdateOK
    _                -> return UpdateBad

checkPSetDropReplay :: PersistentSet ty k -> Version -> k -> Query rw alg a UpdateCheck
checkPSetDropReplay PSet{psetTableName=tbl, ..} version k = do
  r <- query1 ("SELECT tag FROM "<>tbl<>" WHERE key = ? AND ver = ?")
              ( encodeField psetEncodingK k
              , bumpForDB version
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
newtype EphemeralQ alg a dct x = EphemeralQ
  { _unEphemeral :: StateT (dct Overlay) (MaybeT (Query 'RO alg a)) x }
  deriving (Functor, Applicative, Monad, Alternative)

data Overlay a where
  OverlayPMap :: !(PMap k v)
              -> !(Map k (Maybe v))
              -> Overlay (PMap k v)
  OverlayPSet :: !(PersistentSet ty k)
              -> !(Map k Bool)
              -> Overlay (PersistentSet ty k)

overlayPMap :: Lens' (Overlay (PMap k v)) (Map k (Maybe v))
overlayPMap = lens (\(OverlayPMap _ o) -> o) (\(OverlayPMap p _) o -> OverlayPMap p o)

overlayPSet :: Lens' (Overlay (PersistentSet ty k)) (Map k Bool)
overlayPSet = lens (\(OverlayPSet _ o) -> o) (\(OverlayPSet p _) o -> OverlayPSet p o)


-- | Run transaction without changing database. Since it's used for
--   checking transactions or blocks it always uses state
--   corresponding to latest commited block.
runEphemeralQ
  :: (FloatOut dct)
  => dct Persistent          -- ^ Description of user state
  -> EphemeralQ alg a dct x  -- ^ Action to execute
  -> Query 'RO  alg a (Maybe x)
runEphemeralQ dct (EphemeralQ effect) = do
  let overlay = fmapF makeOverlay dct
  runMaybeT $ evalStateT effect overlay



instance ExecutorRO (EphemeralQ alg a) where
  lookupKey getter k = EphemeralQ $ do
    OverlayPMap pmap overlay <- use getter
    case k `Map.lookup` overlay of
      Just r  -> return r
      Nothing -> lift $ lift $ lookupKeyPMap pmap (Commited maxBound) k
  --
  materializePMap getter = EphemeralQ $ do
    OverlayPMap pmap overlay <- use getter
    db <- lift $ lift $ materializePMapWorker pmap (Commited maxBound)
    return $ Map.mapMaybe id $ overlay <> (Just <$> db)
  --
  isMember getter k = EphemeralQ $ do
    OverlayPSet pset overlay <- use getter
    case k `Map.lookup` overlay of
      Just f  -> return f
      Nothing -> lift $ lift $ isMemberPSet pset (Commited maxBound) k
  --
  materializePSet getter = EphemeralQ $ do
    OverlayPSet pset overlay <- use getter
    db <- lift $ lift $ materializePSetWorker pset (Commited maxBound)
    return $ (db <> Map.keysSet (Map.filter id overlay)) `Set.difference`
             Map.keysSet (Map.filter not overlay)


instance ExecutorRW (EphemeralQ alg a) where
  storeKey getter k v = EphemeralQ $ do
    OverlayPMap pmap overlay <- use getter
    -- If we inserted/removed key insert is not valid anyway
    when (k `Map.member` overlay) rollbackEph
    -- Check for insert conflicts in DB
    checkPMapInsertReal pmap k `checkedEph`
      (getter . overlayPMap %= Map.insert k (Just v))
  --
  dropKey getter k = EphemeralQ $ do
    OverlayPMap pmap overlay <- use getter
    case k `Map.lookup` overlay of
      Just Nothing  -> rollbackEph
      Just (Just _) -> getter . overlayPMap %= Map.insert k Nothing
      Nothing       -> checkPMapDropReal pmap k `checkedEph`
        (getter . overlayPMap %= Map.insert k Nothing)
 --
  storeSetElem getter k = EphemeralQ $ do
    OverlayPSet pset overlay <- use getter
    when (k `Map.member` overlay) rollbackEph
    checkPSetStoreReal pset k `checkedEph`
      (getter . overlayPSet %= Map.insert k True)
  --
  dropSetElem getter k = EphemeralQ $ do
    OverlayPSet pset overlay <- use getter
    case k `Map.lookup` overlay of
      Just True  -> getter . overlayPSet %= Map.insert k False
      Just False -> rollbackEph
      Nothing    -> checkPSetDropReal pset k `checkedEph`
        (getter . overlayPSet %= Map.insert k False)

checkedEph
  :: (MonadTrans t, MonadTrans q, Monad (t (q m)), Monad (q m), Monad m)
  => m UpdateCheck -> t (q m) a -> t (q m) a
checkedEph check action = lift (lift check) >>= \case
  UpdateOK   -> action
  UpdateBad  -> fail  "ROLLBACK"
  UpdateNoop -> error "Impossible"

rollbackEph :: StateT (dct Overlay) (MaybeT (Query 'RO alg a)) x
rollbackEph = fail "ROLLBACK"


----------------------------------------------------------------
-- Versioning helpers
----------------------------------------------------------------


tableHead :: PersistentData p => p -> Query rw alg a Version
tableHead a =
  query ("SELECT MAX(ver) FROM "<>tbl) () >>= \case
    [Only Nothing ] -> return New
    [Only (Just v)] -> return (Commited v)
    _               -> error "Impossible"
  where
   tbl = tableName a


versionForHeight
  :: Height                     -- ^ Height of block _being_ evaluated
  -> Persistent x
  -> Query rw alg a (Versioned x)
versionForHeight (Height 0) p = return $ Versioned New p
versionForHeight (Height h) p = do
  r <- query1 "SELECT ver FROM thm_checkpoints WHERE height = ? AND tableName = ?"
              (h-1, persistentTableName p)
  case r of
    -- FIXME: Is calling error correct here???
    Nothing              -> error "Unknown version"
    Just (Only Nothing)  -> return $ Versioned New p
    Just (Only (Just i)) -> return $ Versioned (Commited i) p


storeCheckpoint
  :: Height
  -> Versioned x
  -> Query 'RW alg a ()
storeCheckpoint (Height h) (Versioned ver p) = do
  r <- query1 "SELECT ver FROM thm_checkpoints WHERE height = ? AND tableName = ?"
              (h, persistentTableName p)
  case r of
    Nothing       -> execute "INSERT INTO thm_checkpoints VALUES (?,?,?)"
                             (persistentTableName p, h, v)
    Just (Only v')
      | v /= v'   -> rollback
      | otherwise -> return ()
  where
    v = case ver of New        -> Nothing
                    Commited i -> Just i

makeOverlay :: Persistent a -> Overlay a
makeOverlay = \case
  PersistentPMap p -> OverlayPMap p Map.empty
  PersistentPSet p -> OverlayPSet p Map.empty

persistentTableName :: Persistent p -> Text
persistentTableName = \case
  PersistentPMap x -> tableName x
  PersistentPSet x -> tableName x

persistentCreateTable :: Persistent p -> Query 'RW alg a ()
persistentCreateTable = \case
  PersistentPMap x -> createTable x
  PersistentPSet x -> createTable x
