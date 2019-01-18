{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Parser in the spirit of Monadic Parser Combinators that
-- is compatible with Read class (ReadS/reads). It is safe
-- to use it where you need Read instance - to parse relatively
-- short strings with possible somewhat complex nesting.
--
-- Please note that you can use @many@ and @some@ combinators
-- from @Alternative@ to parse lists.
--
module Thundermint.Utils.Parser (
    P(..)
  , pcheck
  , psepby1
  , pstring
  , pitem
  , pchar
  , pnum
  ) where

import Control.Applicative
import qualified Data.List as List

newtype P a = P { parse :: String -> [(a, String)] }

instance Functor P where
  fmap f (P q) = P $ \s -> [(f a, s') | (a,s') <- q s]

instance Applicative P where
  pure c = P $ \s -> [(c, s)]
  P pf <*> P pa = P $ \s -> [(f a, s'') | (f, s') <- pf s, (a, s'') <- pa s']

instance Alternative P where
  empty = P $ const []
  P f <|> P g = P $ \s -> f s ++ g s

-- |Allows one to contract parses of wrong values.
-- E.g., pcheck (\x -> if wrong x then [] else [x]) ...
-- You also can easily expand values into series of different projections.
pcheck :: (a -> [b]) -> P a -> P b
pcheck f (P q) = P $ \s -> [(y, s') | (x, s') <- q s, y <- f x]

-- |Parse a list of values separated by given delimiter (also parser).
psepby1 :: P d -> P a -> P [a]
psepby1 delim p = (:) <$> p <*> many (delim *> p)

-- |Parse required sequence of characters.
pstring :: String -> P String
pstring [] = pure []
pstring (c:cs) = (:) <$> pitem c <*> pstring cs

-- |Return next available character.
pitem :: Char -> P Char
pitem x = pcheck (\c -> if c == x then [c] else []) pchar

pchar :: P Char
pchar = P $ \s -> case s of
  (c:cs) -> [(c,cs)]
  _      -> []

-- |Parse a string of digits ([1-9][0-9]*|0) and return
-- integer value that can be read from that string.
pnum :: Integral i => P i
pnum = fromIntegral <$> pinteger
  where
    pinteger :: P Integer
    pinteger = read <$> (pstring "0" <|> nonzero)
    nonzero = (:) <$> foldr (<|>) empty (map pitem "123456789") <*> many (foldr (<|>) empty $ map pitem "0123456789")

