-- |
-- Newtype wrappers for deriving instances for mtl's type
-- classes. Their intended use is to generateinstances to be used with
-- other via-deriving newtypes.
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
module HSChain.Control.Via
  (
  ) where

import Control.Monad.Reader
import Data.Generics.Product.Fields (HasField'(..))
import Data.Generics.Product.Typed  (HasType(..))
import Lens.Micro

-- | Convert instance @MonadReader r@ into instance @MonadReader x@.
--   Field @x@ of @r@ is looked up by its type
newtype ReaderViaType x m a = ReaderViaType (m a)
  deriving newtype (Functor,Applicative,Monad)

instance ( MonadReader r m
         , HasType x r
         ) => MonadReader x (ReaderViaType x m) where
  ask = ReaderViaType $ (^. (typed @x)) <$> ask
  local f (ReaderViaType m) = ReaderViaType $ local (typed @x %~ f) m


-- | Convert instance @MonadReader r@ into instance @MonadReader x@.
--   Field @x@ of @r@ is looked up by its selector name @field@.
newtype ReaderViaField field x m a = ReaderViaField (m a)
  deriving newtype (Functor,Applicative,Monad)

instance ( MonadReader r m
         , HasField' field r x
         ) => MonadReader x (ReaderViaField field x m) where
  ask = ReaderViaField $ (^. (field' @field)) <$> ask
  local f (ReaderViaField m) = ReaderViaField $ local (field' @field %~ f) m
