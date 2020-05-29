{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module HSChain.Config.Internal
  ( -- * Data types
    Mangler(..)
  , FromConfigJSON(..)
    -- * Generic-based type classes
  , GFields(..)
  , GConfig(..)
    -- * Helpers
  , TransparentGeneric(..)  
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Aeson
import Data.Aeson.Types
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
  parseConfigJSON :: Mangler -> Value -> Parser a

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
  Mangler f1 g1 <> Mangler f2 g2 = Mangler (liftA2 (.) f1 f2) (g1 <=< g2)

instance Monoid Mangler where
  mempty = Mangler (return id) return


----------------------------------------------------------------
-- Derivation of FromJSON instance for generics
----------------------------------------------------------------

-- | Type class for generating parses for the using 'Generic' type
--   class. Only record data types are supported
class GConfig f where
  parseConfig :: Mangler        -- ^ How to transform fields labels
              -> Value
              -> Parser (f p)

instance (Datatype i, GConfig f) => GConfig (M1 D i f) where
  parseConfig mangler v = M1 <$> prependFailure err (parseConfig mangler v)
    where
      err = printf "while parsing %s defined in %s:%s\n"
              (datatypeName p) (packageName p) (moduleName p)
      p = undefined :: M1 D i f ()

instance ( GConfigRec f
         , GFields f         
         ) => GConfig (M1 C i f) where
  parseConfig m (Object o) = do
    o' <- mangleJsonObj m o
    let fieldsAndTypes = getFields (Proxy @f)
        fieldNames     = fst <$> fieldsAndTypes
        mangler        = evalState (mangleSelector m) fieldNames
        fieldErrs      = [ printf "    + %s : %s\n" (mangler s) (show t) | (s,t) <- fieldsAndTypes ]
    M1 <$> parseRecord mangler fieldErrs o'
  parseConfig _ _          = fail "Expecting JSON object"


-- Parse record. 
class GConfigRec f where
  parseRecord :: (String -> String) -- How to transform labels
              -> [String]           -- List of keys in dictionary (used for error messages)
              -> Object
              -> Parser (f p)

instance ( KnownSymbol s
         , GConfigField f
         ) => GConfigRec (M1 S ('MetaSel ('Just s) su ss ds) f) where
  parseRecord mangle fields o = M1 <$> parseRecField fields field (field `HM.lookup` o)
    where
      field = T.pack $ mangle $ symbolVal (Proxy @s)

instance ( TypeError ('Text "Data type must be a record")
         ) => GConfigRec (M1 S ('MetaSel 'Nothing su ss ds) f) where
  parseRecord = error "Unreachable"

instance ( TypeError ('Text "Sum types are not supported")
         ) => GConfigRec (f :+: g) where
  parseRecord = error "Unreachable"

instance (GConfigRec f, GConfigRec g) => GConfigRec (f :*: g) where
  parseRecord
    = (liftA2 . liftA2 . liftA2 . liftA2) (:*:) parseRecord parseRecord




class GConfigField f where
  parseRecField :: [String] -> T.Text -> Maybe Value -> Parser (f p)

instance {-# OVERLAPPABLE #-} FromJSON a => GConfigField (K1 R a) where
  parseRecField fields fld Nothing
    = (if null fields then id else prependFailure "Records's fields:\n")
    $ foldr prependFailure
      (fail $ "Missing mandatory field \"" ++ T.unpack fld ++ "\"\n")
      fields
  parseRecField _ fld (Just v)
    = prependFailure (" - while parsing field \"" ++ T.unpack fld ++ "\"\n")
    $ K1 <$> parseJSON v
                               
instance FromJSON a => GConfigField (K1 R (Maybe a)) where
  parseRecField _ _   Nothing = return (K1 Nothing)
  parseRecField _ fld (Just v)
    = prependFailure (" - while parsing field \"" ++ T.unpack fld ++ "\"\n")
    $ K1 . Just <$> parseJSON v



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



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Newtype that allows to derive 'Generic' instance which is same as
--   instance of underlying using @DerivingVia@. For example:
--
-- > newtype X a = X a
-- >   deriving Generic via TransparentGeneric (X a)
newtype TransparentGeneric a = TransparentGeneric a

instance Generic a => Generic (TransparentGeneric a) where
  type Rep (TransparentGeneric a) = Rep a
  to   = TransparentGeneric . to
  from = from . (\(TransparentGeneric a) -> a)
