{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- This module contains newtypes which allow easily define 'FromJSON'
-- instances for haskell record which could be used parsing configs
-- either using @aeson@ or @yaml@ libraries. Main focus of this
-- library is to provide instances with good error messages and being
-- easy to write. Performance is secondary priority since config is
-- parsed only once.
module HSChain.Config
  ( Config(..)
  , DropPrefix(..)
  , DropN(..)
  , SnakeCase(..)
  , CaseInsensitive(..)
  ) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.Coerce
import Data.Proxy
import qualified Data.Text           as T
import qualified Data.HashMap.Strict as HM
import GHC.TypeLits
import GHC.Generics

import HSChain.Config.Internal


----------------------------------------------------------------
-- Config newtype
----------------------------------------------------------------

-- | Newtype wrapper which allows to derive 'FromJSON' instance. It
--   allows to generate instance which provides backtrace to error for
--   conversion from JSON.
--
-- > data Cfg = Cfg
-- >   { port :: Word16
-- >   , host :: String
-- >   }
-- >   deriving Generic
-- >   deriving FromJSON via Config Cfg
newtype Config a = Config a
  deriving Generic via TransparentGeneric a

instance (Generic a, GConfig (Rep a)) => FromJSON (Config a) where
  parseJSON = parseConfigJSON mempty

instance (Generic a, GConfig (Rep a)) => FromConfigJSON (Config a) where
  parseConfigJSON m = fmap (Config . to) . parseConfig m


----------------------------------------------------------------
-- Drop prefix
----------------------------------------------------------------

-- | This type allos to define instance that drop common prefix from
--   fields. It tries to be intelligent and uses following algorithm:
--   1) it strips common lower case prefix and stops at first upper
--   case letter or 2) strips common prefix before @'@.
--
--   This is needed in order to avoid corner cases like: @data Cfg =
--   Cfg { cfgPath :: String }@ where common prefix is @cfgPath@ or
--   @data Cfg = Cfg { cfgPath :: String, cfgPort :: Port }@ where
--   common prefix is @cfgP@.
newtype DropPrefix a = DropPrefix a
  deriving Generic via TransparentGeneric a

instance (FromConfigJSON a) => FromJSON (DropPrefix a) where
  parseJSON = parseConfigJSON mempty

instance (FromConfigJSON a) => FromConfigJSON (DropPrefix a) where
  parseConfigJSON m = coerceParser . parseConfigJSON (m <> m')
    where
      m'      = mempty { mangleSelector = mangler }
      mangler = do fields <- get
                   let f | all hasQuote fields = stripQuote
                         | otherwise           = let n = length $ commonPrefix fields
                                                 in lowerHead . drop n
                   selectorMangler f

-- String hash single quote and it's not in tail position.
hasQuote :: String -> Bool
hasQuote ('\'':_ ) = True
hasQuote (_   :xs) = hasQuote xs
hasQuote _         = False

stripQuote :: String -> String
stripQuote = tail . dropWhile (/='\'')


commonPrefix :: [String] -> String
commonPrefix []  = []
commonPrefix str = foldl1 prefix str
  where
    prefix (x:xs) (y:ys)
      | x == y
      , isLower x = x : prefix xs ys
    prefix _ _    = []

lowerHead :: String -> String
lowerHead []     = []
lowerHead (c:cs) = toLower c : cs


----------------------------------------------------------------
-- Drop N letters
----------------------------------------------------------------

-- | Drop N characters from each label
newtype DropN (n :: Nat) a = DropN a
  deriving Generic via TransparentGeneric a

instance (KnownNat n, FromConfigJSON a) => FromJSON (DropN n a) where
  parseJSON = parseConfigJSON mempty

instance (KnownNat n, FromConfigJSON a) => FromConfigJSON (DropN n a) where
  parseConfigJSON m = coerceParser . parseConfigJSON (m' <> m)
    where
      m' = mempty { mangleSelector = selectorMangler f}
      f  = drop (fromIntegral (natVal (Proxy @n)))


----------------------------------------------------------------
-- Convert to snake_case
----------------------------------------------------------------

newtype SnakeCase a = SnakeCase a
  deriving Generic via TransparentGeneric a

instance (FromConfigJSON a) => FromJSON (SnakeCase a) where
  parseJSON = parseConfigJSON mempty

instance (FromConfigJSON a) => FromConfigJSON (SnakeCase a) where
  parseConfigJSON m
    = coerceParser . parseConfigJSON (simpleMangler toSnakeCase <> m)


toSnakeCase :: String -> String
toSnakeCase (c1:c2:cs)
  | isLower c1
  , isUpper c2     = c1 : '_' : toLower c2 : toSnakeCase cs
toSnakeCase (c:cs) = toLower c : toSnakeCase cs
toSnakeCase []     = []


----------------------------------------------------------------
-- Case insensitive
----------------------------------------------------------------

newtype CaseInsensitive a = CaseInsensitive a
  deriving Generic via TransparentGeneric a

instance (FromConfigJSON a) => FromJSON (CaseInsensitive a) where
  parseJSON = parseConfigJSON mempty

instance (FromConfigJSON a) => FromConfigJSON (CaseInsensitive a) where
  parseConfigJSON m = coerceParser . parseConfigJSON (m' <> m)
    where
      m' = Mangler
        { mangleSelector = selectorMangler (map toLower)
        , mangleJsonObj  = foldM add HM.empty . HM.toList
        }
      add o (k,v)
        | k' `HM.member` o = fail $ "Duplicate case-insensitive key: " ++ T.unpack k
        | otherwise        = return $ HM.insert k' v o
        where
          
          k' = T.toLower k


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

coerceParser :: Coercible a (f a) => Parser a -> Parser (f a)
coerceParser = coerce


simpleMangler :: (String -> String) -> Mangler
simpleMangler f = mempty { mangleSelector = selectorMangler f }

selectorMangler :: (String -> String) -> State [String] (String -> String)
selectorMangler f = f <$ modify' (map f)
