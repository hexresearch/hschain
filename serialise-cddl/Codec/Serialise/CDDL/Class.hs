{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Type class for derivation of schema
module Codec.Serialise.CDDL.Class (
    Schema(..)
  , SchemaRep(..)
  , CDDL(..)
  , cddlSchema
  , flattenSchema
  , typeRepToVar
  ) where

import Codec.Serialise
import Control.Lens hiding (rewrite)
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.List
import Data.Int
import Data.Word
import Data.Typeable
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL

import GHC.Generics hiding (UInt)

import Codec.Serialise.CDDL.AST


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Schema for a data type
data Schema a = Schema
  { cddlTypename   :: a
  , cddlSchemaExpr :: SchemaRep a
  }
  deriving (Show,Functor)

-- | Tag which shows whether schema should be inlined or referenced by
--   name
data SchemaRep a
  = Inline (TyExpr Nm (Schema a))
  | ByName (TyExpr Nm (Schema a))
  deriving (Show,Functor)

-- | Type class which allows one to generate schema
class (Typeable a, Serialise a) => CDDL a where
  -- | Get schema for values. This is internal method, use
  --   'cddlSchema' instead.
  cddlRecSchema :: proxy a -> SchemaRep TypeRep
  default cddlRecSchema :: (GCDDL (Rep a)) => proxy a -> SchemaRep TypeRep
  cddlRecSchema _ = gcddlRecSchema (Proxy @(Rep a))
  -- | Schema for list of values
  cddlRecSchemaList :: proxy a -> SchemaRep TypeRep
  cddlRecSchemaList p
    = Inline $ Array $ GrpUnnamed (Times Nothing Nothing) (Named (Nm (cddlSchema p)))

cddlSchema :: CDDL a => proxy a -> Schema TypeRep
cddlSchema p = Schema
  { cddlTypename   = typeRep p
  , cddlSchemaExpr = cddlRecSchema p
  }


flattenSchema :: Ord a => Schema a -> SchemaCDDL Nm a
flattenSchema schema = SchemaCDDL
  { topLevel    = cddlTypename schema
  , bindings    = fmap BindTy
                $ fst
                $ execState (rewrite schema) (Map.empty, Set.empty)
  }
  where
    rewrite Schema{..} = do
      ty <- use _2
      unless (cddlTypename `Set.member` ty) $ do
        _2 %= Set.insert cddlTypename
        r  <- traverse rewrite
            $ inlineTyE
            $ case cddlSchemaExpr of
                Inline e -> e
                ByName e -> e
        _1 %= Map.insert cddlTypename r
      return cddlTypename
    -- Recursively inline all occurences of inline schemas
    inlineTyE e = case e of
      Named (Nm Schema{..}) -> case cddlSchemaExpr of
        Inline x -> inlineTyE x
        ByName _ -> e
      Prim   _  -> e
      Lit    _  -> e
      Array  ng -> Array  $ inlineG ng
      Map    ng -> Map    $ inlineG ng
      Choice c  -> Choice $ inlineTyE <$> c
    --
    inlineG grp = case grp of
      GrpChoice  gs    -> GrpChoice (inlineG <$> gs)
      GrpSeq     gs    -> GrpSeq    (inlineG <$> gs)
      GrpVar{}         -> grp     
      GrpOccur   o g   -> GrpOccur   o   (inlineG   g)
      GrpUnnamed o t   -> GrpUnnamed o   (inlineTyE t)
      GrpNamed   o l t -> GrpNamed   o l (inlineTyE t)


-- Exceedingly ugly conversion of names to CDDL grammar
typeRepToVar :: TypeRep -> String
typeRepToVar = worker False
  where
    worker useBrackets ty
      | [] <- pars  = conNm
      | useBrackets = "$" ++ fullNm ++ "$"
      | otherwise   = fullNm
      where
        conNm      = showTyCon con
        parNms     = worker True <$> pars
        fullNm     = intercalate "_" $ conNm : parNms
        (con,pars) = splitTyConApp ty
    --
    showTyCon con
      | modN == "GHC.Tuple" = "GHC.Tuple"
      | otherwise           = modN ++ "." ++ conN
      where
        modN = tyConModule con
        conN = tyConName   con


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

-- FIXME: ranges for fixed-size integers
instance CDDL Int where
  cddlRecSchema _ = Inline $ Prim Int
instance CDDL Int8 where
  cddlRecSchema _ = Inline $ Prim Int
instance CDDL Int16 where
  cddlRecSchema _ = Inline $ Prim Int
instance CDDL Int32 where
  cddlRecSchema _ = Inline $ Prim Int
instance CDDL Int64 where
  cddlRecSchema _ = Inline $ Prim Int

instance CDDL Word where
  cddlRecSchema _ = Inline $ Prim UInt
instance CDDL Word8 where
  cddlRecSchema _ = Inline $ Prim UInt
instance CDDL Word16 where
  cddlRecSchema _ = Inline $ Prim UInt
instance CDDL Word32 where
  cddlRecSchema _ = Inline $ Prim UInt
instance CDDL Word64 where
  cddlRecSchema _ = Inline $ Prim UInt

instance CDDL Float where
  cddlRecSchema _ = Inline $ Prim Float32
instance CDDL Double where
  cddlRecSchema _ = Inline $ Prim Float64


instance CDDL BS.ByteString where
  cddlRecSchema _ = Inline $ Prim BStr
instance CDDL BL.ByteString where
  cddlRecSchema _ = Inline $ Prim BStr
instance CDDL T.Text where
  cddlRecSchema _ = Inline $ Prim TStr

instance CDDL () where
  cddlRecSchema _ = Inline $ Prim Null

instance (CDDL a, CDDL b) => CDDL (a,b) where
  cddlRecSchema _ = Inline $ Array $ GrpSeq
    [ GrpUnnamed Once (Named (Nm (cddlSchema (Proxy @a))))
    , GrpUnnamed Once (Named (Nm (cddlSchema (Proxy @b))))
    ]

instance CDDL a => CDDL [a] where
  cddlRecSchema _ = cddlRecSchemaList (Proxy @a)



----------------------------------------------------------------
-- Generics
----------------------------------------------------------------

class GCDDL (f :: * -> *) where
  gcddlRecSchema :: Proxy f -> SchemaRep TypeRep

instance GCDDL a => GCDDL (M1 i c a) where
  gcddlRecSchema _ = gcddlRecSchema (Proxy @a)

instance GCDDL U1 where
  gcddlRecSchema _ = ByName $ Array $ GrpUnnamed Once (Lit (LitI 0))

instance CDDL a => GCDDL (K1 i a) where
  gcddlRecSchema _ = ByName $ Array $ GrpSeq
    [ GrpUnnamed Once (Lit (LitI 0))
    , GrpUnnamed Once (Named $ Nm $ cddlSchema (Proxy @a))
    ]

instance (GCDDLProd f, GCDDLProd g) => GCDDL (f :*: g) where
  gcddlRecSchema _ = ByName $ Array
    $  GrpUnnamed Once (Lit (LitI 0))
    <> gcddlRecSchemaProd (Proxy @f)
    <> gcddlRecSchemaProd (Proxy @g)

instance (GCDDLSum f, GCDDLSum g) => GCDDL (f :+: g) where
  gcddlRecSchema _ = ByName $ Choice $ zipWith merge idx cons
    where
      cons      = gcddlRecSchemaSum (Proxy @f) <> gcddlRecSchemaSum (Proxy @g)
      idx       = [GrpUnnamed Once (Lit (LitI i)) | i <- [0 ..]]
      merge i c = Array $ i <> c


class GCDDLProd (f :: * -> *) where
  gcddlRecSchemaProd :: Proxy f -> Group Nm (Schema TypeRep)

instance CDDL a => GCDDLProd (K1 i a) where
  gcddlRecSchemaProd _ = GrpUnnamed Once (Named $ Nm $ cddlSchema (Proxy @a))

instance GCDDLProd U1 where
  gcddlRecSchemaProd _ = mempty

instance GCDDLProd f => GCDDLProd (M1 i c f) where
  gcddlRecSchemaProd _ = gcddlRecSchemaProd (Proxy @f)

instance (GCDDLProd f, GCDDLProd g) => GCDDLProd (f :*: g) where
  gcddlRecSchemaProd _ = gcddlRecSchemaProd (Proxy @f) <> gcddlRecSchemaProd (Proxy @g)



class GCDDLSum (f :: * -> *) where
  gcddlRecSchemaSum :: Proxy f -> [Group Nm (Schema TypeRep)]

instance (GCDDLSum f, GCDDLSum g) => GCDDLSum (f :+: g) where
  gcddlRecSchemaSum _ = gcddlRecSchemaSum (Proxy @f) <> gcddlRecSchemaSum (Proxy @g)

instance (GCDDLProd f) => GCDDLSum (M1 i c f) where
  gcddlRecSchemaSum _ = [gcddlRecSchemaProd (Proxy @f)]
