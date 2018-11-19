{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- |
module TM.Persistence (tests) where

import Control.Monad
import Lens.Micro
import Data.Functor.Compose
import qualified Data.Map.Strict as Map
import           Data.Map.Strict   (Map)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField   (ToField)

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
    in [ testSimple "insert"      dct
           [PMapInsert i i | i <- [1..4]]
       , testSimple "insert/drop" dct
           [ PMapInsert 1 1
           , PMapDrop   1
           , PMapInsert 2 2
           ]
       , testSimple "duplicate insert" dct
           [ PMapInsert 1 1
           , PMapInsert 1 1
           ]
       , testSimple "duplicate drop"   dct
           [ PMapInsert 1 1
           , PMapDrop 1
           , PMapDrop 1
           ]
       , testSimple "drop noexistent"  dct
           [ PMapDrop 1
           ]
       ]
  ]

testSimple :: Model m => String -> One m Persistent -> [Command m] -> TestTree
testSimple name dct cmds = testGroup name
  [ testCase "persistent" $ testModel   dct cmds
  , testCase "ephemeral"  $ testEpheral dct cmds
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
               => Repr m -> q (One m) (IO ())

testModel :: Model m => One m Persistent -> [Command m] -> IO ()
testModel dct cmds = do
  conn <- openDatabase ":memory:" dct genesis validatorSet
  let model = foldM evalModel initialModel cmds
  res <- runQueryRW conn (runBlockUpdate (Height 0) dct (mapM_ evalStore cmds))
  case (model, res) of
    (Nothing,Nothing) -> return ()
    (Nothing,Just ()) -> assertFailure "Model failed while evaluation didn't!"
    (Just _ ,Nothing) -> assertFailure "Model is OK while evaluation failed"
    (Just m, Just ()) -> join $ runQueryRO conn $ queryUserState (Height 0) dct $ check m

testEpheral :: Model m => One m Persistent -> [Command m] -> IO ()
testEpheral dct cmds = do
  conn <- openDatabase ":memory:" dct genesis validatorSet
  let model = foldM evalModel initialModel cmds
  res <- runQueryRO conn $ runEphemeralQ dct $ do
    mapM_ evalStore cmds
    case model of
      Just m  -> check m
      Nothing -> return $ assertFailure "Model failed while evaluation didn't!"
  case (model, res) of
    (_      , Just m ) -> m
    (Nothing, Nothing) -> return ()
    (Just _ , Nothing) -> assertFailure "Model is OK while evaluation failed"


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
  check (PMapRepr m) = do
    res <- materializePMap one
    tst <- forM (Map.toList m) $ \(k,v) -> do
      v' <- lookupKey one k
      return $ assertEqual ("For key: " ++ show k) v v'
    return $ sequence_
      $ (Map.mapMaybe id m @=? res)
      : tst
