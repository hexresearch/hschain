{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Crypto.Bls.Internal where


-- NB: Partially copied from haskell-opencv

import Control.Exception (mask_)
import Data.List ( intercalate )
import Data.Monoid ( (<>) )
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import Foreign.Ptr (Ptr, FunPtr)
import Language.Haskell.TH
import Language.Haskell.TH.Quote ( quoteExp )
import qualified Data.Map as M
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Types  as C


data C'AggregationInfo
data C'InsecureSignature
data C'PrivateKey
data C'PublicKey
data C'Signature
data C'Threshold


blsTypesTable :: C.TypesTable
blsTypesTable = M.fromList
  [ ( C.TypeName "AggregationInfo",   [t| C'AggregationInfo   |] )
  , ( C.TypeName "InsecureSignature", [t| C'InsecureSignature |] )
  , ( C.TypeName "PrivateKey",        [t| C'PrivateKey        |] )
  , ( C.TypeName "PublicKey",         [t| C'PublicKey         |] )
  , ( C.TypeName "Signature",         [t| C'Signature         |] )
  , ( C.TypeName "Threshold",         [t| C'Threshold         |] )
  , ( C.TypeName "bool",              [t| C.CInt              |] )
  ]


blsCtx :: C.Context
blsCtx = C.cppCtx <> C.bsCtx <> C.vecCtx <> ctx
  where
    ctx = mempty { C.ctxTypesTable = blsTypesTable }




-- | Information about the storage requirements of values in C
--
-- This class assumes that the type @a@ is merely a symbol that corresponds with
-- a type in C.
class CSizeOf a where
    -- | Computes the storage requirements (in bytes) of values of
    -- type @a@ in C.
    cSizeOf :: proxy a -> Int



objFromPtr :: (ForeignPtr c -> hask)  -- ^ Constructor from pointer
           -> FunPtr (Ptr c -> IO ()) -- ^ Finalizer
           -> IO (Ptr c)              -- ^ Object
           -> IO hask                 -- ^ Result
objFromPtr haskCons finalizer mkObjPtr = mask_ $ do
    objPtr <- mkObjPtr
    haskCons <$> newForeignPtr finalizer objPtr









-- | Equivalent type in C
--
-- Actually a proxy type in Haskell that stands for the equivalent type in C.
type family C (a :: *) :: *









-- | Copy source to destination using C++'s placement new feature
class PlacementNew a where
    -- | Copy source to destination using C++'s placement new feature
    --
    -- This method is intended for types that are proxies for actual
    -- types in C++.
    --
    -- > new(dst) CType(*src)
    --
    -- The copy should be performed by constructing a new object in
    -- the memory pointed to by @dst@. The new object is initialised
    -- using the value of @src@. This design allow underlying
    -- structures to be shared depending on the implementation of
    -- @CType@.
    placementNew
        :: Ptr a -- ^ Source
        -> Ptr a -- ^ Destination
        -> IO ()

    placementDelete
        :: Ptr a
        -> IO ()








{- | Generates a function that deletes a C++ object via @delete@.

Example:

> mkFinalizer "deleteFoo" "CFoo" ''Foo

Generated code (stylized):

> 'C.verbatim' "extern \"C\""
> {
>   void deleteFoo(CFoo * obj)
>   {
>     delete obj;
>   }
> }

> foreign import ccall "&deleteFoo" deleteFoo :: FunPtr (Ptr Foo -> IO ())

-}
mkFinalizer :: String -> String -> Name -> DecsQ
mkFinalizer name cType haskellCType = do
    finalizerImportDec <- finalizerImport
    cFinalizerDecs <- C.verbatim cFinalizerSource
    pure $ finalizerImportDec : cFinalizerDecs
  where
    finalizerImport :: DecQ
    finalizerImport =
        forImpD CCall Safe ("&" <> name) (mkName name)
          [t| FunPtr (Ptr $(conT haskellCType) -> IO ()) |]
    cFinalizerSource :: String
    cFinalizerSource =
        intercalate "\n" $
          [ "extern \"C\""
          , "{"
          , "  void " <> name <> "(" <> cType <> " * obj)"
          , "  {"
          , "    delete obj;"
          , "  }"
          , "}"
          ]


mkPlacementNewInstance :: Name -> DecsQ
mkPlacementNewInstance name =
    [d|
      instance PlacementNew $(conT (mkName ctypeName)) where
          placementNew    = $(placementNewQ)
          placementDelete = $(placementDeleteQ)
    |]
  where
    typeName = nameBase name
    ctypeName = "C'"  <> typeName

    placementNewQ = do
      src <- newName "src"
      dst <- newName "dst"
      lamE [varP src, varP dst] $
        quoteExp C.exp $
          "void { new($(" <> typeName <> " * " <> nameBase dst <> ")) bls::" <> typeName <>
                   "(*$(" <> typeName <> " * " <> nameBase src <> ")) }"

    placementDeleteQ = do
      ptr <- newName "ptr"
      lamE [varP ptr] $
        quoteExp C.exp $
          "void { $(" <> typeName <> " * " <> nameBase ptr <> ")->~" <> typeName <> "() }"
