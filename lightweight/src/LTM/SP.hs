{-# LANGUAGE BangPatterns, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

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

{-
instance Applicative (SP i) where
  pure = flip O N
  O f spF <*> spA = spF <*> spA <|> fmap f spA
  N       <*> _   = N
  _       <*> N   = N
  I f     <*> spA = I (\i -> f i <*> spA)

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
  fail _ = N

class HasInput m i | m -> i where
  input :: m i

instance HasInput (SP i) i where
  input = I return

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
                           O o sp' -> withOutput o sp'
                           I g' -> wait g'
-}

type SPE i l a = SP i (Either l a)

infixr 1 >>=^, >>^
(>>=^) :: SPE i l a -> (a -> SPE i l b) -> SPE i l b
O (Left l) sp >>=^ q = O (Left l) $ sp >>=^ q
O (Right a) N >>=^ q = q a
O (Right a) sp >>=^ q = error "alternative???" --(sp >>=^ q) <|> q a
I f >>=^ q = I $ (>>=^ q) . f
N >>=^ _ = N

(>>^) :: SPE i l a -> SPE i l b -> SPE i l b
a >>^ b = a >>=^ const b

out :: o -> SP i (Either o ())
out o = O (Left o) $ O (Right ()) N


mapSPE :: (a -> SPE i l b) -> [a] -> SPE i l [b]
mapSPE f [] = O (Right []) N
mapSPE f (x:xs) = f x >>=^ \y -> mapSPE f xs >>=^ \ys -> O (Right (y:ys)) N

mapSPE_ :: (a -> SPE i l ()) -> [a] -> SPE i l ()
mapSPE_ f [] = O (Right ()) N
mapSPE_ f (x:xs) = f x >>=^ \_ -> mapSPE_ f xs >>=^ \_ -> O (Right ()) N

forSPE :: [a] -> (a -> SPE i l b) -> SPE i l [b]
forSPE = flip mapSPE

forSPE_ :: [a] -> (a -> SPE i l ()) -> SPE i l ()
forSPE_ = flip mapSPE_

input :: SP i i
input = I $ flip O N

{-
newtype SPB i o a = SPB { runSPB :: SP i (Either o a)}

instance Functor (SPB i o) where
  fmap f = SPB . fmap (fmap f) . runSPB

instance Applicative (SPB i o) where
  pure = SPB . return . Right
  ~(SPB spf) <*> ~(SPB spa) = SPB $ liftA2 (<*>) spf spa

instance Alternative (SPB i o) where
  empty = SPB empty
  ~(SPB a) <|> ~(SPB b) = SPB $ a <|> b

instance Monad (SPB i o) where
  return = pure
  ~(SPB spa) >>= q = SPB $ case spa of
    N -> N
    O (Left o) sp -> O (Left o) $ runSPB (SPB sp >>= q)
    O (Right r) sp -> runSPB (q r) <|> runSPB (SPB sp >>= q)
    I f -> I $ runSPB . ((>>= q) . SPB) . f

instance HasInput (SPB i o) i where
  input = SPB $ fmap Right input

takeApply :: [i] -> SP i o -> [o]
takeApply xs (O o sp) = o : takeApply xs sp
takeApply xs N = []
takeApply [] _ = []
takeApply (x:xs) (I f) = takeApply xs (f x)
-}