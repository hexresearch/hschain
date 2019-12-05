module HSChain.SQL where

import Control.Monad
import Control.Monad.State

import qualified Data.ByteString as BS

import Language.SQL.SimpleSQL.Dialect
import Language.SQL.SimpleSQL.Parse
import Language.SQL.SimpleSQL.Pretty
import Language.SQL.SimpleSQL.Syntax


----------------------------------------------------------------
-- |General handling of SQL statements and associated data.
----------------------------------------------------------------

-- |Safe string value escaping.
sqlStr :: String -> String
sqlStr s = '\'' : concatMap escape s ++ "'"
  where
    escape '\'' = "''"
    escape c = [c]

-- |SQL code generation monad. Not even newtype-worthy.
type SQLGenMonad a = StateT [String] IO a

-- |SQL dialect of choice - for compatibility reasons we do not use
-- any vendor-specific dialect.
sqlDialect :: Dialect
sqlDialect = ansi2011

-- |Add statements into code generation state after validating them.
addStatements :: [String] -> SQLGenMonad ()
addStatements [] = return ()
addStatements ss = do
  let txt = unlines ss
  case parseStatements sqlDialect "" Nothing $ unlines ss of
    Left err -> error $ "internal error: statements parsing failure: " ++ show err++ "\nstatements:\n--------------\n" ++ unlines ss ++"--------------"
    Right _ -> modify $ \ssPrev  -> ssPrev ++ ss

-- |Statement normalization - we remove any control character by
-- turning it into space and then reduce spaces to single space.
-- This effectively makes single-line statement from possible
-- multiline pretty-printed one.
normalizeStatementString :: String -> String
normalizeStatementString cs = reduceSpaces $ onlySpaces cs
  where
    onlySpaces = map (\c -> if c < ' ' then ' ' else c)
    reduceSpaces (' ':' ':cs) = reduceSpaces $ ' ':cs
    reduceSpaces (c:cs) = c : reduceSpaces cs 
    reduceSpaces cs = cs

----------------------------------------------------------------
-- |Signatures
--
-- We use homegrown textual encoding of signatures for transactions
-- which are text, essentially.
--
-- Text is split in lines (NL as separator, text must end in NL)
-- and first three lines are:
--   1. public key used to verify signature
--   2. signature (see definition of @Alg@ above) of the following text and
--   3. salt - any string would do, but remember that we deduplicate
--      transactions on salts alone.
--
-- All other lines of text are left intact and bear no significance
-- for signature creation/validation.
--
----------------------------------------------------------------

checkTextualSignature :: BS.ByteString -> BS.ByteString
                       -> BS.ByteString -> [BS.ByteString]
                       -> Bool
checkTextualSignature publickey signature salt textLines = True

