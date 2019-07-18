{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
-- |
module Codec.Serialise.CDDL.Parser where

-- import Control.Applicative
import Control.Monad
import Data.Char
import Data.List (foldl1')
import Data.Foldable
import qualified Data.Map.Strict as Map
import Text.Megaparsec
import Text.Megaparsec.Char
import Codec.Serialise.CDDL.AST


----------------------------------------------------------------
--
----------------------------------------------------------------

type Parser = Parsec String String

cddl :: Parser (SchemaCDDL Nm Var)
cddl = do
  b:bs           <- ws *> some (rule <* ws) <* eof
  SchemaCDDL{..} <- foldM merge b bs
  m              <- traverse (substituteGrpVar bindings) bindings
  return SchemaCDDL{ bindings = m
                   , ..
                   }
  where
    merge (SchemaCDDL nm bA) (SchemaCDDL _ bB)
      | Map.null $ Map.intersection bA bB = return $ SchemaCDDL nm (bA <> bB)
      | otherwise                         = fail "Duplicate name"


-- Substitute group variables as groups. We parse variables without
-- names as GrpUnnamed since we can't distinguish between GrpUnnamed
-- and GrpVar and substitute during postprocessing. At the same time
-- we check that all variables are defined
substituteGrpVar
  :: Map.Map Var (Binding Nm a)
  -> Binding Nm Var
  -> Parser (Binding Nm Var)
substituteGrpVar defs = \case
  BindTy  t -> BindTy  <$> substTy t
  BindGrp g -> BindGrp <$> substG  g
  where
    substTy ty = case ty of
      Prim{}    -> pure ty
      Lit{}     -> pure ty
      Named (Nm v)  -> case v `Map.lookup` defs of
        Just BindTy{}  -> pure ty
        Just BindGrp{} -> fail "Group references where name was expected"
        Nothing        -> fail "Unknown name"
      Array  x  -> Array  <$> substG x
      Map    x  -> Map    <$> substG x
      Choice xs -> Choice <$> traverse substTy xs
    substG grp = case grp of
      GrpChoice  gs    -> GrpChoice <$> traverse substG gs
      GrpSeq     gs    -> GrpSeq    <$> traverse substG gs
      GrpVar _ (Nm v)  -> case v `Map.lookup` defs of
        Just BindTy{}  -> fail "Type references where group was expected"
        Just BindGrp{} -> pure grp
        Nothing        -> fail "Unknown name"
      GrpOccur   o   x -> GrpOccur o <$> substG x
      -- Special case
      GrpUnnamed _ (Named (Nm v)) -> case v `Map.lookup` defs of
        Just BindTy{}  -> pure grp
        Just BindGrp{} -> undefined
        Nothing        -> fail "Unknown name"
      GrpUnnamed o   x -> GrpUnnamed o   <$> substTy x
      GrpNamed   o l x -> GrpNamed   o l <$> substTy x
      
      

----------------------------------------------------------------
-- Parser proper
----------------------------------------------------------------

rule :: Parser (SchemaCDDL Nm Var)
rule = do
  name <- (resolveTypename <$> identifier) >>= \case
    Left  _  -> fail "Illegal type name"
    Right nm -> return nm
  -- FIXME: generics
  -- FIXME: support for /= & //=
  () <$ ws <* char '=' <* ws
  b <-  fmap BindTy typeExpr
    <|> bindingGrp
  return SchemaCDDL { topLevel = name
                    , bindings = Map.singleton name b
                    }


typeExpr :: Parser (TyExpr Nm Var)
typeExpr = do
  ty <- (:) <$> typedef
            <*> many (char '/' *> ws *> typedef)
  case ty of
    [t] -> return t
    _   -> return $ Choice ty


typedef :: Parser (TyExpr Nm Var)
typedef = asum
  [ char '(' *> ws *> typeExpr         <* ws <* char ')'
  , char '[' *> ws *> fmap Array group <* ws <* char ']'
  , char '{' *> ws *> fmap Map   group <* ws <* char '}'
  , do nm <- identifier
       return $ case resolveTypename nm of
         Left  p -> Prim  p
         Right n -> Named (Nm n)
  , Lit <$> value
  ] <* ws


group :: Parser (Group Nm Var)
group = do
  gs <- (:) <$> groupchoice
            <*> many (char '/' *> char '/' *> ws *> groupchoice)
  case gs of
    [g] -> return g
    _   -> return $ GrpChoice gs

groupchoice :: Parser (Group Nm Var)
groupchoice =
  many groupent >>= \case
    [g] -> return g
    gs  -> return $ GrpSeq gs
  
groupent :: Parser (Group Nm Var)
groupent = do
  o <- occur <* ws
  asum [ char '(' *> ws *> (GrpOccur o <$> group) <* ws <* char ')'
       , do -- FIXME: We need to support keys as values!
            mlab <- optional $ try (identifier <* ws <* string ":" <* ws)
            t    <- typeExpr
            return $ case mlab of
              Nothing -> GrpUnnamed o   t
              Just l  -> GrpNamed   o l t
       ] <* ws

bindingGrp :: Parser (Binding Nm Var)
bindingGrp = fail "bindingGrp: not implemented"

resolveTypename :: Var -> Either Prim Var
resolveTypename = \case
  "bool"    -> Left Bool
  "uint"    -> Left UInt
  "nint"    -> Left NInt
  "int"     -> Left Int
  "float16" -> Left Float16
  "float32" -> Left Float32
  "float64" -> Left Float64
  "float"   -> Left Float
  "bstr"    -> Left BStr
  "bytes"   -> Left BStr
  "tstr"    -> Left TStr
  "text"    -> Left TStr
  "null"    -> Left Null
  i         -> Right i

occur :: Parser Occur
occur = asum
  [ OnePlus <$ char '+'
  , ZeroOne <$ char '?'
  , try $ Times <$> optional uint <* char '*' <*> optional uint
  , pure Once
  ]


value :: Parser Literal
-- FIXME: non-integer literals
value = LitI <$> digit

int,uint :: Parser Integer
uint = asum
  [ do d  <- digit1
       ds <- many digit
       return $! foldl' (\n i -> n*10 + i) d ds
  , do char '0'*> asum
         [ do ds <- char 'x' *> some hexdigit
              return $! foldl1' (\n i -> n*16 + i) ds
         , do ds <- char 'b' *> some bindigit
              return $! foldl1' (\n i -> n*16 + i) ds
         , pure 0
         ]
  ]
int = asum
  [ negate <$> (char '-' *> uint)
  , uint
  ]

digit,digit1,hexdigit,bindigit :: Parser Integer
digit1   = fromIntegral . digitToInt <$> satisfy (\c -> c >= '1' && c <= '9')
digit    = fromIntegral . digitToInt <$> satisfy (\c -> c >= '0' && c <= '9')
bindigit = fromIntegral . digitToInt <$> satisfy (\c -> c >= '0' && c <= '1')
hexdigit = fromIntegral . digitToInt <$> satisfy (\c ->
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))

identifier :: Parser Var
identifier = (:) <$> ealpha <*> many (ealpha <|> digitC <|> oneOf "-.")

alpha,ealpha,digitC :: Parser Char
alpha  = satisfy $ \c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
ealpha = alpha <|> oneOf "@_$"
digitC = satisfy $ \c -> (c >= '0' && c <= '9')

comment :: Parser ()
comment = () <$ char ';' <* many pchar <* (void eol <|> eof)

pchar :: Parser Char
pchar = satisfy $ \c -> c >= chr 0x20 && c /= chr 0x7F

ws :: Parser ()
ws = space *> (comment *> ws <|> pure ())
