{-# LANGUAGE BangPatterns #-}

module LTM.SP
  ( module LTM.SP
  , module LTM.Types
  ) where

import Control.Applicative

import LTM.Types

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

infixl 1 `transCompose`

-- |Compose processors in the transducers' sense.
transCompose :: SP i a -> SP a o -> SP i o
transCompose N _ = N
transCompose _ N = N
transCompose a (O o b) = O o $ transCompose a b
transCompose (O a spa) (I f) = transCompose spa (f a <|> I f)
transCompose (I f) b = I $ flip transCompose b . f

-- |Folding combinator. Runs accumulating function
-- over outputs of processor. Please not that
-- this combinator outputs not more than single output.
foldSP :: (o -> a -> o) -> o -> SP i a -> SP i o
foldSP f o N = return o
foldSP f o (O a sp) = foldSP f (f o a) sp
foldSP f o (I g) = I $ \x -> foldSP f o (g x)

-- |Scan - runs undelying processor and
-- apply accumulating function to the outputs,
-- producing new outputs of accumulated values.
-- If there were N outputs by argument processor,
-- there will be N+1 outputs by the combinator.
scanSP :: (o -> a -> o) -> o -> SP i a -> SP i o
scanSP f o N = return o
scanSP f o (O a sp) = O o $ scanSP f (f o a) sp
scanSP f o (I g) = I $ \i -> scanSP f o $ g i

-- |Above - a combinator that takes input
-- of type Either upEvent dnEvent. Nothing flags termination.
-- It passes all dnEvents to processor below and
-- returns last output of processor below.
above :: (ctrl -> Bool) -> SP i o -> SP (UpDown ctrl i) o
above termP sp = case sp of
  N -> N
  O o sp -> withOutput o sp
  I g -> wait g
  where
    withOutput !o N = pure o
    withOutput _ (O o sp) = withOutput o sp
    withOutput o (I g) = I $ \i -> case i of
                             Up upEv
                               | termP upEv -> pure o
                               | otherwise -> withOutput o (I g)
                             Down dnEv -> withOutput o (g dnEv)
    wait g = I $ \i -> case i of
                         Up upEv
                           | termP upEv -> empty
                           | otherwise -> wait g
                         Down dnEv -> case g dnEv of
                           N -> empty
                           O o sp -> withOutput o sp
                           I g' -> wait g'
