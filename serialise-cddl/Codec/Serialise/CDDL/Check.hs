{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Checker of schema
module Codec.Serialise.CDDL.Check (
    check
  ) where

import Codec.CBOR.Term
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Foldable

import Codec.Serialise.CDDL.AST


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Check 
check :: TyExpr Rec a -> Term -> Bool
check ty x = case ty of
  Prim p -> checkPrim p x
  Lit  p -> checkLit  p x
  Named (Rec ty') -> check ty' x
  Array g -> case x of
    TList  xs -> checkArrGrp g xs
    TListI xs -> checkArrGrp g xs
    _         -> False
  Map _       -> error "Maps aren't supported"
  Choice ts   -> any (flip check x) ts

checkPrim :: Prim -> Term -> Bool
checkPrim p t = case p of
  Int -> case t of
    TInt{} -> True
    _      -> False
  UInt -> case t of
    TInt{} -> True
    _      -> False
  NInt -> case t of
    TInt{} -> True
    _       -> False
  Float16 -> case t of
    THalf{} -> True
    _       -> False
  Float32 -> case t of
    TFloat{} -> True
    _        -> False
  Float64 -> case t of
    TDouble{} -> True
    _         -> False
  Float -> case t of
    THalf{}   -> True
    TFloat{}  -> True
    TDouble{} -> True
    _         -> False
  BStr -> case t of
    TBytes{}  -> True
    TBytesI{} -> True
    _         -> False
  TStr -> case t of
    TString{}  -> True
    TStringI{} -> True
    _          -> False
  Bool -> case t of
    TBool{} -> True
    _       -> False
  Null -> case t of
    TNull -> True
    _     -> False

checkLit :: Literal -> Term -> Bool
-- checkLit p x | traceShow ("checkLit",p,x) False = undefined
checkLit (LitI i) (TInt j) = i == fromIntegral j
checkLit _ _ = False


newtype GP a = GP (StateT [Term] Maybe a)
  deriving (Functor,Applicative,Monad,Alternative)

matchGP :: [Term] -> GP a -> Bool
matchGP ts (GP m) = case execStateT m ts of
  Just [] -> True
  _       -> False



checkArrGrp :: Group Rec a -> [Term] -> Bool
checkArrGrp grp ts = matchGP ts $ matchGroup grp

matchGroup :: Group Rec a -> GP ()
matchGroup = \case
  GrpChoice  gs        -> asum      $ matchGroup <$> gs
  GrpSeq     gs        -> for_ gs   $ matchGroup
  GrpVar     o (Rec g) -> occurGP o $ matchGroup g
  GrpOccur   o   g     -> occurGP o $ matchGroup g
  GrpUnnamed o   ty    -> occurGP o $ matchTy ty
  GrpNamed   o _ ty    -> occurGP o $ matchTy ty

matchTy :: TyExpr Rec a -> GP ()
matchTy ty = GP $ do
  t:ts <- get
  -- traceShow ("GET",t,ts) $ return ()
  guard $ check ty t
  -- traceShow ("PUT",ts) $ return ()
  put ts
  
occurGP :: Occur -> GP a -> GP ()
occurGP o gp = case o of
  Once      -> void gp
  OnePlus   -> void $ some gp
  ZeroOne   -> void $ optional gp
  Times Nothing  Nothing  -> void $ many gp
  Times (Just i) Nothing  -> void $ replicateM_ (fromIntegral i) gp *> many gp
  Times Nothing  (Just k) -> upto k gp
  Times (Just i) (Just k) -> replicateM_ (fromIntegral i) gp *> upto (k - i) gp
  where
    upto 0 _ = empty
    upto n m = empty <|> void m *> upto (n-1) m
