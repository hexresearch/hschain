{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- |
module TM.Persistence (tests) where

import Control.Monad
import Lens.Micro
import Data.Foldable
import Data.Int
import Data.Functor.Compose
import qualified Data.Map.Strict as Map
import           Data.Map.Strict   (Map)
import qualified Data.Set        as Set
import           Data.Set          (Set)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField   (ToField)
import Text.Printf

import Test.Tasty
import Test.Tasty.HUnit

import Thundermint.Blockchain.Types
import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Crypto.Ed25519
import Thundermint.Mock.KeyList
import Thundermint.Store
import Thundermint.Store.SQL
import Thundermint.Store.Internal.Query
import Thundermint.Store.Internal.Types


tests :: TestTree
tests = testGroup "Tests for persistent data"
  [ testGroup "PMap" $
    let dct = dict :: One (PMap Int Int) Persistent
    in [ testCase "insert" $ exhaustiveTest dct
           [ [PMapInsert i i | i <- [1..4]]
           ]
       , testCase "insert/drop" $ exhaustiveTest dct
           [ [ PMapInsert 1 1
             , PMapDrop   1
             , PMapInsert 2 2
             ]
           ]
       , testCase "multiple stages" $ exhaustiveTest dct
           [ [ PMapInsert 1 1
             , PMapDrop   1
             , PMapInsert 2 2
             ]
           , [ PMapInsert 3 3
             , PMapDrop   2
             ]
           , [ PMapInsert 4 4
             ]
           ]
       , testCase "duplicate insert" $ exhaustiveTest dct
           [ [ PMapInsert 1 1
             , PMapInsert 1 1
             ]
           ]
       , testCase "duplicate drop"   $ exhaustiveTest dct
           [ [ PMapInsert 1 1
             , PMapDrop 1
             , PMapDrop 1
             ]
           ]
       , testCase "drop nonexistent"  $ exhaustiveTest dct
           [ [ PMapDrop 1
             ]
           ]
       ]
  ----------------------------------------
  , testGroup "PSet'" $
    let dct = dict :: One (PSet' Int) Persistent
    in [ testCase "insert" $ exhaustiveTest dct
           [ [ PSetAdd' i | i <- [1..4]]
           ]
       , testCase "multiple stages" $ exhaustiveTest dct
           [ [ PSetAdd' 1
             ]
           , [ PSetAdd' 2
             , PSetAdd' 3
             ]
           , [ PSetAdd' 4
             ]
           ]
       , testCase "duplicate insert" $ exhaustiveTest dct
           [ [ PSetAdd' 1
             , PSetAdd' 1
             ]
           ]
       ]
  , testGroup "PSet" $
    let dct = dict :: One (PSet Int) Persistent
    in [ testCase "insert" $ exhaustiveTest dct
           [ [ PSetAdd i | i <- [1..4]]
           ]
       , testCase "duplicate insert" $ exhaustiveTest dct
           [ [ PSetAdd  1
             , PSetAdd  2
             , PSetDrop 1
             ]
           ]
       , testCase "multiple stages" $ exhaustiveTest dct
           [ [ PSetAdd  1
             , PSetDrop 1
             , PSetAdd  2
             ]
           , [ PSetAdd  3
             , PSetDrop 2
             ]
           , [ PSetAdd  4
             ]
           ]
       , testCase "insert/drop" $ exhaustiveTest dct
           [ [ PSetAdd 1
             , PSetAdd 1
             ]
           ]
       , testCase "drop nonexistent" $ exhaustiveTest dct
           [ [ PSetDrop 1
             ]
           ]
       , testCase "duplicate drop" $ exhaustiveTest dct
           [ [ PSetAdd  1
             , PSetDrop 1
             , PSetDrop 1
             ]
           ]
       ]
  ]

genesis :: Block Ed25519_SHA512 ()
genesis = Block
  { blockHeader = Header
      { headerChainID        = "TEST"
      , headerHeight         = Height 0
      , headerTime           = Time 0
      , headerLastBlockID    = Nothing
      , headerValidatorsHash = hash validatorSet
      }
  , blockData       = ()
  , blockLastCommit = Nothing
  }

validatorSet :: ValidatorSet Ed25519_SHA512
validatorSet = makeValidatorSetFromPriv [ PrivValidator k | k <- take 4  privateKeyList ]

----------------------------------------------------------------
-- Common code
----------------------------------------------------------------

newtype One a f = One (f a)

one :: Lens' (One a f) (f a)
one = lens (\(One x) -> x) (\_ -> One)

instance FunctorF (One a) where
  fmapF f (One x) = One (f x)
instance FloatOut (One a) where
  floatOut (One (Compose x)) = fmap One x

----------------------------------------------------------------
-- Models for persistent data
----------------------------------------------------------------

class (Eq (Repr m), Show (Repr m)) =>  Model m where
  data Repr   m
  data Command m
  initialModel :: Repr m
  dict         :: One m Persistent
  evalStore    :: (ExecutorRW q, Monad (q (One m)))
               => Command m -> q (One m) ()
  evalModel    :: Repr m -> Command m -> Maybe (Repr m)
  check        :: (ExecutorRO q, Monad (q (One m)))
               => String -> Int64 -> Repr m -> q (One m) (IO ())

-- | Runs exhaustive test for persistent data structure.
--
--   - We run ephemeral test for each commit
--   - We commit data
--   - After we commited data we try to replay it
exhaustiveTest :: Model m => One m Persistent -> [[Command m]] -> IO ()
exhaustiveTest dct cmds = do
  withDatabase ":memory:" dct genesis validatorSet $ \conn -> do
    let evald = evaluateModel cmds
    iterateCommands True  conn dct evald
    iterateCommands False conn dct evald

evaluateModel :: (Model m) => [[Command m]] -> [(Int64, Maybe (Repr m), [Command m])]
evaluateModel = zipWith (\h (a,b) -> (h,a,b)) [0..] . go initialModel
  where
    go _ [] = []
    go m (cmds:rest) = case foldM evalModel m cmds of
      Just m' -> (Just m', cmds) : go m' rest
      Nothing -> case rest of
        [] -> (Nothing, cmds) : []
        _  -> error "evaluateModel: there are commands to evaluate"

iterateCommands
  :: (Model m)
  => Bool -> Connection 'RW alg a -> One m Persistent -> [(Int64, Maybe (Repr m), [Command m])] -> IO ()
iterateCommands _ _ _ [] = return ()
iterateCommands runEph conn dct ((h,model,cmds):rest) = do
  -- Test ephemeral update
  when runEph $ do
     res <- runQueryRO conn $ runEphemeralQ dct $ do
       mapM_ evalStore cmds
       case model of
        Just m  -> check "Ephemeral" h m
        Nothing -> return $ assertFailure $ printf "Ephemeral: Model failed, eval OK (H=%i)" h
     case (model, res) of
       (_      , Just m ) -> m
       (Nothing, Nothing) -> return ()
       (Just _ , Nothing) -> assertFailure $ printf "Ephemeral: Model OK, eval failed (H=%i)" h
  -- Do real update
  do res <- runQueryRW conn (runBlockUpdate (Height h) dct (mapM_ evalStore cmds))
     case (model, res) of
       (Nothing,Nothing) -> return ()
       (Nothing,Just ()) -> assertFailure "Model failed while evaluation didn't!"
       (Just _ ,Nothing) -> assertFailure "Model is OK while evaluation failed"
       (Just m, Just ()) -> do join $ runQueryRO conn $ queryUserState (Height h) dct $ check "Persistent" h m
                               iterateCommands runEph conn dct rest



----------------------------------------------------------------
-- Concrete implementations
----------------------------------------------------------------

instance ( Ord k, Ord v
         , Show k, Show v
         , FromField k, FromField v
         , ToField k, ToField v
         ) => Model (PMap k v) where
  newtype Repr    (PMap k v) = PMapRepr (Map k (Maybe v))
                             deriving (Eq,Show)
  data    Command (PMap k v) = PMapInsert k v
                             | PMapDrop   k
  initialModel = PMapRepr mempty
  dict         = One $ wrap $ PMap { pmapTableName = "test"
                                   , pmapEncodingK = FieldEncoding id id
                                   , pmapEncodingV = FieldEncoding id id
                                   }
  --
  evalStore    = \case
    PMapInsert k v -> storeKey one k v
    PMapDrop   k   -> dropKey  one k
  --
  evalModel (PMapRepr m) = \case
    PMapInsert k v  -> case k `Map.lookup` m of
      Just _        -> Nothing
      Nothing       -> Just $ PMapRepr $ Map.insert k (Just v) m
    PMapDrop k      -> case k `Map.lookup` m of
      Just (Just _) -> Just $ PMapRepr $ Map.insert k Nothing m
      _             -> Nothing
  --
  check label h (PMapRepr m) = do
    res <- materializePMap one
    tst <- forM (Map.toList m) $ \(k,v) -> do
      v' <- lookupKey one k
      return $ assertEqual (printf "(%s,H=%i) For key: %s" label h (show k)) v v'
    return $ sequence_
      $ assertEqual (printf "%s, H=%i" label h) (Map.mapMaybe id m) res
      : tst


instance (Ord k, Show k, FromField k, ToField k
         ) => Model (PersistentSet 'AppendOnlySet k) where
  newtype Repr    (PersistentSet 'AppendOnlySet k) = PSetA (Set k)
                                                   deriving (Eq,Show)
  data    Command (PersistentSet 'AppendOnlySet k) = PSetAdd' k
  --
  initialModel = PSetA mempty
  dict         = One $ wrap $ PSet { psetTableName = "test"
                                   , psetEncodingK = FieldEncoding id id
                                   }
  --
  evalStore    = \case
    PSetAdd' k -> storeSetElem one k
  --
  evalModel (PSetA m) = \case
    PSetAdd' k | k `Set.member` m -> Nothing
               | otherwise        -> Just $ PSetA $ Set.insert k m
  --
  check label h (PSetA m) = do
    res <- materializePSet one
    tst <- forM (toList m) $ \k -> do
      b <- isMember one k
      return $ assertBool (printf "Key %s must be present (%s, H=%i)" (show k) label h) b
    return $ sequence_
      $ assertEqual (printf "%s, H=%i" label h) m res
      : tst


instance (Ord k, Show k, FromField k, ToField k
         ) => Model (PersistentSet 'StandardSet k) where
  newtype Repr    (PersistentSet 'StandardSet k) = PSetR (Map k Bool)
                                                   deriving (Eq,Show)
  data    Command (PersistentSet 'StandardSet k) = PSetAdd  k
                                                 | PSetDrop k
  --
  initialModel = PSetR mempty
  dict         = One $ wrap $ PSet { psetTableName = "test"
                                   , psetEncodingK = FieldEncoding id id
                                   }
  --
  evalStore = \case
    PSetAdd  k -> storeSetElem one k
    PSetDrop k -> dropSetElem  one k
  --
  evalModel (PSetR m) = \case
    PSetAdd k   -> case k `Map.lookup` m of
      Just _    -> Nothing
      Nothing   -> return $ PSetR $ Map.insert k True m
    PSetDrop k  -> case k `Map.lookup` m of
      Just True -> return $ PSetR $ Map.insert k False m
      _         -> Nothing
  --
  check label h (PSetR m) = do
    res <- materializePSet one
    tst <- forM (Map.toList m) $ \(k,v) -> do
      b <- isMember one k
      return $ assertEqual (printf "Key %s must be (%s, H=%i)" (show k) label h) v b
    return $ sequence_
      $ assertEqual (printf "%s H=%i" label h) (Map.keysSet (Map.filter id m)) res
      : tst
