-- |Common types.

module LTM.Types where

-- |Same as Either, defined for better readability.
data UpDown up down = Up up | Down down deriving (Eq, Ord, Show)

instance Functor (UpDown up) where
  fmap f (Up up) = Up up
  fmap f (Down dn) = Down $ f dn

instance Applicative (UpDown up) where
  pure = Down
  Up up <*> _ = Up up
  Down f <*> Up up = Up up
  Down f <*> Down x = Down $ f x

instance Monad (UpDown up) where
  return = pure
  Up up >>= _ = Up up
  Down dn >>= g = g dn

