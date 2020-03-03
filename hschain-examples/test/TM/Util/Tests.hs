-- | Tests for peer exchange
--
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module TM.Util.Tests where

import           Control.Monad
import           Data.Set   (Set)
import qualified Data.Set as Set

import           Test.Tasty.HUnit


assertSubset :: (Ord a, Show a, HasCallStack)
             => String
             -> Set a
             -> Set a
             -> Assertion
assertSubset preface expected actual =
    unless (expected `Set.isSubsetOf` actual) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n but got: " ++ show actual


infix 1 @<?

(@<?) :: (Ord a, Show a, HasCallStack)
      => Set a
      -> Set a
      -> Assertion
expected @<? actual = assertSubset "" expected actual
