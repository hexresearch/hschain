-- |Stream processors extended with
-- monadic actions.

{-# LANGUAGE GADTs #-}

module LTM.SPM where

import Control.Applicative

data SP m i o where
  N :: SP m i o
  -- ^Null processor.
  O :: o -> (SP m i o) -> SP m i o
  -- ^A processor that outputs something.
  I :: (i -> SP m i o) -> SP m i o
  -- ^A processor that waits for input and then process it.
  A :: m a -> (a -> SP m i o) -> SP m i o
  -- ^Expanded monadic bind.

instance Functor (SP m i) where
  fmap f N = N
  fmap f (I g) = I $ fmap (fmap f) g
  fmap f (O o sp) = O (f o) (fmap f sp)
  fmap f (A a g) = A a (fmap f . g)

instance Applicative (SP m i) where
  pure = flip O N
  O f spF <*> spA = spF <*> spA <|> fmap f spA
  N       <*> _   = N
  _       <*> N   = N
  A a f   <*> spA = A a $ \x -> f x <*> spA
  I f     <*> I g = I (\i -> f i <*> g i)

instance Alternative (SP m i) where
  empty = N
  N       <|> spb     = spb
  spa     <|> N       = spa
  O o spa <|> spb     = O o (spa <|> spb)
  spa     <|> O o spb = O o (spa <|> spb)
  A a f   <|> spb     = A a $ \x -> f x <|> spb
  I fa    <|> I fb    = I $ \i -> fa i <|> fb i

instance Monad (SP m i) where
  return = pure
  N      >>= _ = N
  O o sp >>= g = g o <|> (sp >>= g)
  I f    >>= g = I $ \i -> f i >>= g
  A a f  >>= g = A a $ \x -> f x >>= g
