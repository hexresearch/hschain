{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- This module contains newtypes which allow easily define 'FromJSON'
-- instances for haskell record which could be used parsing configs
-- either using @aeson@ or @yaml@ libraries. Main focus of this
-- library is to provide instances with good error messages and being
-- easy to write. Performance is secondary priority since config is
-- parsed only once.
module HSChain.Config.Internal.Impl
  ( Config(..)
  , DropSmart(..)
  , manglerDropSmart
  , DropN(..)
  , manglerDropN
  , DropPrefix(..)
  , manglerDropPrefix
  , SnakeCase(..)
  , manglerSnakeCase
  , CaseInsensitive(..)
  , manglerCaseInsensitive
  , WithDefault(..)
    -- * Helpers
  , coerceParser
  , simpleMangler
  , selectorMangler
  ) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.Coerce
import Data.Default.Class
import Data.Proxy
import qualified Data.Text           as T
import qualified Data.HashMap.Strict as HM
import GHC.TypeLits
import GHC.Generics

import HSChain.Config.Internal.Classes


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
  deriving newtype (Generic, Default)

instance (Generic a, GConfig (Rep a)) => FromJSON (Config a) where
  parseJSON = parseConfigJSON mempty Nothing

instance (Generic a, GConfig (Rep a)) => FromConfigJSON (Config a) where
  parseConfigJSON m a
    = fmap (Config . to) . parseConfig m (from <$> a)


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
newtype DropSmart a = DropSmart a
  deriving newtype (Default,Generic)

instance (FromConfigJSON a) => FromJSON (DropSmart a) where
  parseJSON = parseConfigJSON mempty Nothing

instance (FromConfigJSON a) => FromConfigJSON (DropSmart a) where
  parseConfigJSON m a
    = coerceParser . parseConfigJSON (m <> manglerDropSmart) (coerce a)

manglerDropSmart :: Mangler
manglerDropSmart = mempty { mangleSelector = mangler }
  where
    mangler = do fields <- get
                 let n    = length  $ commonPrefix fields
                     maxN = maximum $ map length fields
                       -- All identitfiers has single quote in them
                     f | all hasQuote fields = stripQuote
                       -- For some reason we're tryting to strip everything
                       | n >= maxN           = id
                       | otherwise           = lowerHead . drop n
                 selectorMangler f

-- String hash single quote and it's not in tail position.
hasQuote :: String -> Bool
hasQuote ('\'':_ ) = True
hasQuote (_   :xs) = hasQuote xs
hasQuote _         = False

stripQuote :: String -> String
stripQuote = tail . dropWhile (/='\'')


commonPrefix :: [String] -> String
commonPrefix []        = []
commonPrefix str       = foldl1 prefix str
  where
    -- We treat leading prefix specially since we want to strip it too
    prefix ('_':xs) ('_':ys) = '_' : go xs ys
    prefix xs       ys       = go xs ys
    -- When computing prefix we want to break on first uppercase
    -- letter or underscore
    go ('_':_) ('_':_) = "_"
    go (x:xs)  (y:ys)
      | x == y
      , isLower x = x : go xs ys
    go _ _    = []

lowerHead :: String -> String
lowerHead []     = []
lowerHead (c:cs) = toLower c : cs


----------------------------------------------------------------
-- Drop given prefix
----------------------------------------------------------------

-- | Drop given prefix from accessor labels. Prefix is supplied as
--   type level literal.
newtype DropPrefix (s :: Symbol) a = DropPrefix a
  deriving newtype (Default)

instance (KnownSymbol s, FromConfigJSON a) => FromJSON (DropPrefix s a) where
  parseJSON = parseConfigJSON mempty Nothing

instance (KnownSymbol s, FromConfigJSON a) => FromConfigJSON (DropPrefix s a) where
  parseConfigJSON m a
    = coerceParser . parseConfigJSON (m <> manglerDropPrefix s) (coerce a)
    where
      s = symbolVal (Proxy @s)

manglerDropPrefix :: String -> Mangler
manglerDropPrefix s = simpleMangler (dropStr s)
  where
    dropStr (a:as) (b:bs) | a == b = dropStr as bs
    dropStr _      bs              = bs


----------------------------------------------------------------
-- Drop N letters
----------------------------------------------------------------

-- | Drop N characters from each label
newtype DropN (n :: Nat) a = DropN a
  deriving newtype (Default,Generic)

instance (KnownNat n, FromConfigJSON a) => FromJSON (DropN n a) where
  parseJSON = parseConfigJSON mempty Nothing

instance (KnownNat n, FromConfigJSON a) => FromConfigJSON (DropN n a) where
  parseConfigJSON m a
    = coerceParser . parseConfigJSON (m <> manglerDropN n) (coerce a)
    where
      n  = fromIntegral $ natVal (Proxy @n)

manglerDropN :: Int -> Mangler
manglerDropN n = simpleMangler (drop n)


----------------------------------------------------------------
-- Convert to snake_case
----------------------------------------------------------------

-- | Convert @camelCase@ field accessors to @snake_case@.
newtype SnakeCase a = SnakeCase a
  deriving newtype (Default,Generic)

instance (FromConfigJSON a) => FromJSON (SnakeCase a) where
  parseJSON = parseConfigJSON mempty Nothing

instance (FromConfigJSON a) => FromConfigJSON (SnakeCase a) where
  parseConfigJSON m a
    = coerceParser . parseConfigJSON (m <> manglerSnakeCase) (coerce a)

manglerSnakeCase :: Mangler
manglerSnakeCase = simpleMangler toSnakeCase

toSnakeCase :: String -> String
toSnakeCase (c1:c2:cs)
  | isLower c1
  , isUpper c2     = c1 : '_' : toLower c2 : toSnakeCase cs
toSnakeCase (c:cs) = toLower c : toSnakeCase cs
toSnakeCase []     = []


----------------------------------------------------------------
-- Case insensitive
----------------------------------------------------------------

-- | Make keys in JSON object field accessors case insensitive. Having
--   confiling keys will result in parse error.
newtype CaseInsensitive a = CaseInsensitive a
  deriving newtype (Default,Generic)

instance (FromConfigJSON a) => FromJSON (CaseInsensitive a) where
  parseJSON = parseConfigJSON mempty Nothing

instance (FromConfigJSON a) => FromConfigJSON (CaseInsensitive a) where
  parseConfigJSON m a
    = coerceParser . parseConfigJSON (m <> manglerCaseInsensitive) (coerce a)

manglerCaseInsensitive :: Mangler
manglerCaseInsensitive = Mangler
  { mangleSelector = selectorMangler (map toLower)
  , mangleJsonObj  = foldM add HM.empty . HM.toList
  }
  where
    add o (k,v)
      | k' `HM.member` o = fail $ "Duplicate case-insensitive key: " ++ T.unpack k
      | otherwise        = return $ HM.insert k' v o
      where
        k' = T.toLower k


----------------------------------------------------------------
-- With default
----------------------------------------------------------------

newtype WithDefault a = WithDefault a
  deriving newtype (Default)

instance (Default a, FromConfigJSON a) => FromJSON (WithDefault a) where
  parseJSON = parseConfigJSON mempty Nothing

instance (Default a, FromConfigJSON a) => FromConfigJSON (WithDefault a) where
  parseConfigJSON m _
    = coerceParser . parseConfigJSON m (Just def)

  
----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

coerceParser :: Coercible a (f a) => Parser a -> Parser (f a)
coerceParser = coerce

simpleMangler :: (String -> String) -> Mangler
simpleMangler f = mempty { mangleSelector = selectorMangler f }

selectorMangler :: (String -> String) -> State [String] (String -> String)
selectorMangler f = f <$ modify' (map f)
