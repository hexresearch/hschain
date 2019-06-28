module LTM.SP where

import Control.Applicative

data SP i o =
     N
  -- ^Null processor.
  |  O o (SP i o)
  -- ^A processor that outputs something.
  |  I (i -> SP i o)
  -- ^A processor that waits for input and then process it.

instance Functor (SP i) where
  fmap f N = N
  fmap f (I g) = I $ fmap (fmap f) g
  fmap f (O o sp) = O (f o) (fmap f sp)

instance Applicative (SP i) where
  pure = flip O N
  O f spF <*> spA = spF <*> spA <|> fmap f spA
  N       <*> _   = N
  _       <*> N   = N
  I f     <*> I g = I (\i -> f i <*> g i)

instance Alternative (SP i) where
  empty = N
  N       <|> spb     = spb
  spa     <|> N       = spa
  O o spa <|> spb     = O o (spa <|> spb)
  spa     <|> O o spb = O o (spa <|> spb)
  I fa    <|> I fb    = I $ \i -> fa i <|> fb i

instance Monad (SP i) where
  return = pure
  N      >>= _ = N
  O o sp >>= g = g o <|> (sp >>= g)
  I f    >>= g = I $ \i -> f i >>= g
