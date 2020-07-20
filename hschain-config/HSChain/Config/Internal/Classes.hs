{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module HSChain.Config.Internal.Classes
  ( -- * Data types
    Mangler(..)
  , FromConfigJSON(..)
    -- * Generic-based type classes
  , GFields(..)
  , GConfig(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Aeson
import Data.Aeson.Types
import Data.Coerce
import Data.Typeable
import qualified Data.Text           as T
import qualified Data.HashMap.Strict as HM
import Text.Printf
import GHC.TypeLits
import GHC.Generics


----------------------------------------------------------------
-- Mangling of field selector names
----------------------------------------------------------------

class FromConfigJSON a where
  parseConfigJSON :: Mangler -> Maybe a -> Value -> Parser a

-- | Description on how to transform labels of haskell record to keys
--   in JSON object
data Mangler = Mangler
  { mangleSelector :: State [String] (String -> String)
    -- ^ Function to transform labels of haskell's record
  , mangleJsonObj  :: HM.HashMap T.Text Value -> Parser (HM.HashMap T.Text Value)
    -- ^ Transformation of JDON object before it's being parsed.. This
    --   is needed in order to support things like case insensitive
    --   parsers.
  }

instance Semigroup Mangler where
  Mangler mf1 g1 <> Mangler mf2 g2 = Mangler
    { mangleSelector = do
        f2 <- mf2
        f1 <- mf1
        return (f1 . f2)
    , mangleJsonObj  = g1 <=< g2
    }

instance Monoid Mangler where
  mempty = Mangler (return id) return


----------------------------------------------------------------
-- Derivation of FromJSON instance for generics
----------------------------------------------------------------

-- | Type class for generating parses for the using 'Generic' type
--   class. Only record data types are supported
class GConfig f where
  parseConfig :: Mangler        -- ^ How to transform fields labels
              -> Maybe (f p)
              -> Value
              -> Parser (f p)

instance (GConfig f) => GConfig (M1 D i f) where
  parseConfig mangler a v = M1 <$> parseConfig mangler (coerce a) v

instance ( GConfigRec f
         , GFields f         
         ) => GConfig (M1 C i f) where
  parseConfig m a (Object o) = do
    o' <- mangleJsonObj m o
    let fieldsAndTypes = getFields (Proxy @f)
        fieldNames     = fst <$> fieldsAndTypes
        mangler        = evalState (mangleSelector m) fieldNames
        fieldErrs      = [ printf "    + %s : %s\n" (mangler s) (show t) | (s,t) <- fieldsAndTypes ]
    M1 <$> parseRecord mangler fieldErrs (coerce a) o'
  parseConfig _ _ _ = fail "Expecting JSON object"


-- Parse record. 
class GConfigRec f where
  parseRecord :: (String -> String) -- How to transform labels
              -> [String]           -- List of keys in dictionary (used for error messages)
              -> Maybe (f p)
              -> Object
              -> Parser (f p)

instance ( KnownSymbol s
         , GConfigField f
         ) => GConfigRec (M1 S ('MetaSel ('Just s) su ss ds) f) where
  parseRecord mangle fields a o
    = M1 <$> parseRecField fields field (coerce a) (field `HM.lookup` o)
    where
      field = T.pack $ mangle $ symbolVal (Proxy @s)

instance ( TypeError ('Text "Data type must be a record")
         ) => GConfigRec (M1 S ('MetaSel 'Nothing su ss ds) f) where
  parseRecord = error "Unreachable"

instance ( TypeError ('Text "Sum types are not supported")
         ) => GConfigRec (f :+: g) where
  parseRecord = error "Unreachable"

instance (GConfigRec f, GConfigRec g) => GConfigRec (f :*: g) where
  parseRecord fun keys Nothing o
    = liftA2 (:*:) (parseRecord fun keys Nothing o)
                   (parseRecord fun keys Nothing o)
  parseRecord fun keys (Just (f :*: g)) o
    = liftA2 (:*:) (parseRecord fun keys (Just f) o)
                   (parseRecord fun keys (Just g) o)



class GConfigField f where
  parseRecField :: [String] -> T.Text -> Maybe (f p) -> Maybe Value -> Parser (f p)

instance {-# OVERLAPPABLE #-} (FromJSON a, Typeable a) => GConfigField (K1 R a) where
  parseRecField fields fld Nothing Nothing
    = (if null fields then id else prependFailure "Records's fields:\n")
    $ flip (foldr prependFailure) fields
    $ fail $ "Missing mandatory field \"" ++ T.unpack fld ++ "\"\n"
  parseRecField _ _ (Just a) Nothing
    = pure a
  parseRecField _ fld _ (Just v)
    = prependFailure (errorMsg fld (typeRep (Proxy @a)))
    $ K1 <$> parseJSON v
                               
instance {-# INCOHERENT #-} (FromJSON a, Typeable a) => GConfigField (K1 R (Maybe a)) where
  parseRecField _ _   Nothing  Nothing = return (K1 Nothing)
  parseRecField _ _   (Just a) Nothing = return a
  parseRecField _ fld _        (Just v)
    = prependFailure (errorMsg fld (typeRep (Proxy @a)))
    $ K1 . Just <$> parseJSON v

errorMsg :: T.Text -> TypeRep -> String
errorMsg fld ty = printf " - field \"%s\" : %s\n" (T.unpack fld) (show ty)

----------------------------------------------------------------
-- Generics utils
----------------------------------------------------------------

-- | Extract field names and their types from haskell record
class GFields (f :: * -> *) where
  getFields :: Proxy f -> [(String,TypeRep)]

instance ( GFields f ) => GFields (M1 D i f) where
  getFields _ = getFields (Proxy @f)

instance ( GFields f ) => GFields (M1 C i f) where
  getFields _ = getFields (Proxy @f)

instance (GFields f, GFields g) => GFields (f :*: g) where
  getFields _ = getFields (Proxy @f) ++ getFields (Proxy @g)

instance ( TypeError ('Text "Cannot derive GFields for sum type")
         ) => GFields (f :+: g) where
  getFields = error "Unreachable"

instance ( KnownSymbol s
         , GFieldType f
         ) => GFields (M1 S ('MetaSel ('Just s) su ss ds) f) where
  getFields _ = [ (symbolVal (Proxy @s), getFieldType (Proxy @f)) ]


-- Extract field type
class GFieldType (f :: * -> *) where
  getFieldType :: Proxy f -> TypeRep

instance (Typeable a) => GFieldType (K1 r a) where
  getFieldType _ = typeRep (Proxy @a)
