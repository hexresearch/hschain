{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
module TM.Persistence (tests) where

import Control.Monad
import Lens.Micro
import Data.Functor.Compose
import qualified Data.Map.Strict as Map
import           Data.Map.Strict   (Map)

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
  [ testGroup "PMap"
    [ testCase "insert"      $ comparePMap defPMap [PMapInsert i i | i <- [1..4]]
    , testCase "insert/drop" $ comparePMap defPMap
        [ PMapInsert 1 1
        , PMapDrop   1
        , PMapInsert 2 2
        ]
    , testCase "duplicate insert" $ comparePMap defPMap
        [ PMapInsert 1 1, PMapInsert 1 1]
    , testCase "duplicate drop"   $ comparePMap defPMap
        [ PMapInsert 1 1, PMapDrop 1, PMapDrop 1]
    , testCase "drop noexistent"  $ comparePMap defPMap
        [ PMapDrop 1]
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
-- Model implementation of PMap
----------------------------------------------------------------

defPMap :: One (PMap Int Int) Persistent
defPMap = One $ wrap $ PMap { pmapTableName = "test"
                            , pmapEncodingK = FieldEncoding id id
                            , pmapEncodingV = FieldEncoding id id
                            }



-- | Single command to
data PMapCmd k v
  = PMapInsert k v
  | PMapDrop   k

type PMapModel k v = Map k (Maybe v)

evalPMapModel :: Ord k => [PMapCmd k v] -> Maybe (PMapModel k v)
evalPMapModel = foldM stepPMap Map.empty

stepPMap :: Ord k => PMapModel k v -> PMapCmd k v -> Maybe (PMapModel k v)
stepPMap m = \case
  PMapInsert k v  -> case k `Map.lookup` m of
    Just _        -> Nothing
    Nothing       -> Just $ Map.insert k (Just v) m
  PMapDrop k      -> case k `Map.lookup` m of
    Just (Just _) -> Just $ Map.insert k Nothing m
    _             -> Nothing

evalPMapStore
  :: (Monad (q (One (PMap k v))), ExecutorRW q, Ord k, Eq v)
  => [PMapCmd k v]
  -> q (One (PMap k v)) ()
evalPMapStore = mapM_ $ \case
  PMapInsert k v -> storeKey one k v
  PMapDrop   k   -> dropKey  one k

comparePMap
  :: (Ord k, Eq v, Show k, Show v)
  => One (PMap k v) Persistent -> [PMapCmd k v] -> IO ()
comparePMap dct cmds = do
  let model = evalPMapModel cmds
  conn <- openDatabase ":memory:" dct genesis validatorSet
  runQueryRW conn (runBlockUpdate (Height 0) dct (evalPMapStore cmds)) >>= \case
    Nothing -> assertEqual "Model when evaluation failed" Nothing model
    Just () -> do
      impl <- runQueryRO conn (queryUserState (Height 0) dct (materializePMap one))
      (Map.mapMaybe id <$> model) @=? Just impl
