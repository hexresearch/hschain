{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Abstract syntax tree for CDDL
module Codec.Serialise.CDDL.AST (
    -- * Top-level definitions
    Var
  , Label
  , SchemaCDDL(..)
    -- ** Rest of AST
  , Binding(..)
  , TyExpr(..)
  , Group(..)
  , Occur(..)
  , Prim(..)
  , Literal(..)
  , Nm(..)
    -- * Recursive
  , Rec(..)
  , resolveNames
  ) where

import Data.List
import Data.Foldable
import Prettyprinter
import qualified Data.Map.Strict as Map


----------------------------------------------------------------
-- AST
----------------------------------------------------------------

-- | Variable name
type Var = String

-- | Label of field in the group
type Label = String

-- | Parse result of CDDL specification file
data SchemaCDDL f a = SchemaCDDL
  { topLevel    :: a
  , bindings    :: Map.Map a (Binding f a)
  }

newtype Nm f a = Nm a
  deriving (Show,Eq,Functor,Foldable,Traversable)

-- | Top level bindings. It could be either group or type
data Binding f a
  = BindTy  (TyExpr f a)
  | BindGrp (Group  f a)

-- | Type expression
data TyExpr f a
  = Prim    Prim          -- ^ Primitive type
  | Lit     Literal
  | Named  (f (TyExpr f) a)   -- ^ Type referenced by name
  | Array  (Group  f a)      -- ^ Array referencing
  | Map    (Group  f a)      -- ^ Map
  | Choice [TyExpr f a]     -- ^ Choice between two type expressiosn
  -- deriving (Show,Eq) -- ,Functor,Foldable,Traversable)

-- | Group of defiitions
data Group f a
  = GrpChoice [Group f a]         -- ^ Choice between two groups
  | GrpSeq    [Group f a]         -- ^ Sequence of fields
  | GrpVar     Occur       (f (Group f) a)
  | GrpOccur   Occur       (Group  f a)
  | GrpUnnamed Occur       (TyExpr f a)
    -- ^ Note that it could correspond to group instead of type if
    --   type expression is
  | GrpNamed   Occur Label (TyExpr f a)

-- | Occurence of field
data Occur
  = Once
  | OnePlus
  | ZeroOne
  | Times (Maybe Integer) (Maybe Integer)
  deriving (Show,Eq)

-- | Primitive values
data Prim
  = Bool
  | UInt
  | NInt
  | Int
  | Float16
  | Float32
  | Float64
  | Float
  | BStr
  | TStr
  | Null
  deriving (Show,Eq)

data Literal
  = LitI Integer
  deriving (Show,Eq)


instance Semigroup (Group f a) where
  GrpSeq g1 <> GrpSeq g2 = GrpSeq (g1 <> g2)
  g1        <> GrpSeq [] = g1
  g1        <> GrpSeq g2 = GrpSeq (g1 : g2)
  GrpSeq [] <> g2        = g2
  GrpSeq g1 <> g2        = GrpSeq (g1 <> [g2])
  g1        <> g2        = GrpSeq [g1,g2]

instance Monoid (Group f a) where
  mempty = GrpSeq []


----------------------------------------------------------------
-- Specialized traversals
----------------------------------------------------------------

class FoldableCDDL t where
  -- | Analog of for_
  foldAstM_
    :: (Applicative m)
    => (f (TyExpr f) a -> m ())
    -> (f (Group  f) a -> m ())
    -> t f a
    -> m ()
  substAst
    :: (f (TyExpr f) a -> g (TyExpr g) b)
    -> (f (Group  f) a -> g (Group  g) b)
    -> t f a
    -> t g b

instance FoldableCDDL Binding where
  foldAstM_ f g = \case
    BindTy  x -> foldAstM_ f g x
    BindGrp x -> foldAstM_ f g x
  substAst f g = \case
    BindTy  x -> BindTy  $ substAst f g x
    BindGrp x -> BindGrp $ substAst f g x

instance FoldableCDDL TyExpr where
  foldAstM_ f g = \case
    Prim{}    -> pure ()
    Lit{}     -> pure ()
    Named  v  -> f v
    Array  x  -> foldAstM_ f g x
    Map    x  -> foldAstM_ f g x
    Choice xs -> for_ xs $ foldAstM_ f g
  substAst f g = \case
    Prim   x  -> Prim x
    Lit    x  -> Lit x
    Named  v  -> Named $ f v
    Array  x  -> Array  $ substAst f g x
    Map    x  -> Map    $ substAst f g x
    Choice xs -> Choice $ substAst f g <$> xs

instance FoldableCDDL Group where
  foldAstM_ f g = \case
    GrpChoice  gs    -> for_ gs $ foldAstM_ f g
    GrpSeq     gs    -> for_ gs $ foldAstM_ f g
    GrpVar     _   v -> g v
    GrpOccur   _   x -> foldAstM_ f g x
    GrpUnnamed _   x -> foldAstM_ f g x
    GrpNamed   _ _ x -> foldAstM_ f g x
  substAst f g = \case
    GrpChoice  gs    -> GrpChoice $ substAst f g <$> gs
    GrpSeq     gs    -> GrpSeq    $ substAst f g <$> gs
    GrpVar     o   v -> GrpVar     o   (g v)
    GrpOccur   o   x -> GrpOccur   o   $ substAst f g x
    GrpUnnamed o   x -> GrpUnnamed o   $ substAst f g x
    GrpNamed   o l x -> GrpNamed   o l $ substAst f g x


----------------------------------------------------------------
-- Recursive variant
----------------------------------------------------------------

newtype Rec f a = Rec (f a)

-- | Turn expression into
resolveNames :: (Ord a) => SchemaCDDL Nm a -> TyExpr Rec a
resolveNames SchemaCDDL{..} =
  case Map.lookup topLevel defs of
    Just (BindTy  r) -> r
    Just (BindGrp _) -> error "resolveNames: top level ID refers to unknown type"
    Nothing          -> error "resolveNames: top level ID unknown"
  where
    defs          = substAst substT substG <$> bindings
    substT (Nm v) = case Map.lookup v defs of
      Just (BindTy r) -> Rec r
      _               -> error "resolveNames: unknown variable name"
    substG (Nm v) = case Map.lookup v defs of
      Just (BindGrp r) -> Rec r
      _                -> error "resolveNames: unknown group name"


----------------------------------------------------------------
-- Pretty printer
----------------------------------------------------------------

instance (Ord a, Pretty a, Pretty (f (TyExpr f) a), Pretty (f (Group f) a)) => Pretty (SchemaCDDL f a) where
  -- FIXME: we don't support named groups yet
  pretty SchemaCDDL{..}
    = vsep
    $ prettyT topLevel (bindings Map.! topLevel)
    : [ prettyT v expr
      | (v,expr) <- Map.toList bindings
      , v /= topLevel
      ]
    where
      prettyT v e =  vsep [ pretty v <+> "="
                          , indent 4 $ pretty e
                          ] <> line

instance (Pretty a, Pretty (f (TyExpr f) a), Pretty (f (Group f) a)) => Pretty (Binding f a) where
  pretty = \case
    BindTy  t -> pretty t
    BindGrp g -> vsep ["(", indent 4 $ pretty g, ")"]

instance (Pretty a, Pretty (f (TyExpr f) a), Pretty (f (Group f) a)) => Pretty (TyExpr f a) where
  pretty = \case
    Prim  p  -> pretty p
    Lit   l  -> pretty l
    Named v  -> pretty v
    Array g  -> vsep $ ["[", indent 4 $ pretty g, "]"]
    Map _    -> error "Maps are not implemeted"
    Choice cs -> vsep $ intersperse "/" $ map pretty cs

instance (Pretty a, Pretty (f (TyExpr f) a), Pretty (f (Group f) a)) => Pretty (Group f a) where
  pretty = \case
    -- FIXME: precedence!
    GrpChoice  gs    -> vsep $ intersperse "//" $ map pretty gs
    GrpSeq     gs    -> vsep $ map pretty gs
    GrpVar     o v   -> pretty o <> pretty v
    GrpOccur   o v   -> pretty o <> vsep ["(", indent 4 $ pretty v, ")"]
    GrpUnnamed o t   -> pretty o <> pretty t
    GrpNamed   o l t -> pretty o <> pretty l <> ": " <> pretty t


instance Pretty Occur where
  pretty = \case
    Once      -> mempty
    ZeroOne   -> "?"
    OnePlus   -> "+"
    Times n m -> pretty n <> "*" <> pretty m

instance Pretty Literal where
  pretty = \case
    LitI i -> pretty i

instance Pretty Prim where
  pretty = \case
    Bool    -> "bool"
    UInt    -> "uint"
    NInt    -> "nint"
    Int     -> "int"
    Float16 -> "float16"
    Float32 -> "float32"
    Float64 -> "float64"
    Float   -> "float"
    BStr    -> "bstr"
    TStr    -> "tstr"
    Null    -> "null"

instance Pretty a => Pretty (Nm f a) where
  pretty (Nm a) = pretty a


deriving instance (Show a, Show (f (TyExpr f) a), Show (f (Group f) a)) => Show (SchemaCDDL f a)
deriving instance (Eq   a, Eq   (f (TyExpr f) a), Eq   (f (Group f) a)) => Eq   (SchemaCDDL f a)

deriving instance (Show (f (TyExpr f) a), Show (f (Group f) a)) => Show (Binding f a)
deriving instance (Eq   (f (TyExpr f) a), Eq   (f (Group f) a)) => Eq   (Binding f a)
deriving instance (Functor     (f (TyExpr f)), Functor     (f (Group f))) => Functor     (Binding f)
deriving instance (Foldable    (f (TyExpr f)), Foldable    (f (Group f))) => Foldable    (Binding f)
deriving instance (Traversable (f (TyExpr f)), Traversable (f (Group f))) => Traversable (Binding f)

deriving instance (Show (f (TyExpr f) a), Show (f (Group f) a)) => Show (TyExpr f a)
deriving instance (Eq   (f (TyExpr f) a), Eq   (f (Group f) a)) => Eq   (TyExpr f a)
deriving instance (Functor     (f (TyExpr f)), Functor     (f (Group f))) => Functor     (TyExpr f)
deriving instance (Foldable    (f (TyExpr f)), Foldable    (f (Group f))) => Foldable    (TyExpr f)
deriving instance (Traversable (f (TyExpr f)), Traversable (f (Group f))) => Traversable (TyExpr f)

deriving instance (Show (f (TyExpr f) a), Show (f (Group f) a)) => Show (Group f a)
deriving instance (Eq   (f (TyExpr f) a), Eq   (f (Group f) a)) => Eq   (Group f a)
deriving instance (Functor     (f (TyExpr f)), Functor     (f (Group f))) => Functor     (Group f)
deriving instance (Foldable    (f (TyExpr f)), Foldable    (f (Group f))) => Foldable    (Group f)
deriving instance (Traversable (f (TyExpr f)), Traversable (f (Group f))) => Traversable (Group f)
