module Main where

import Control.Monad
import Control.Monad.State

import System.Exit (exitSuccess, exitFailure)

import Language.SQL.SimpleSQL.Dialect
import Language.SQL.SimpleSQL.Parse
import Language.SQL.SimpleSQL.Pretty
import Language.SQL.SimpleSQL.Syntax

import Options.Applicative

-- |Safe string value escaping.
sqlStr :: String -> String
sqlStr s = '\'' : concatMap escape s ++ "'"
  where
    escape '\'' = "''"
    escape c = [c]

walletDemoTableName :: String
walletDemoTableName = "wallet"

type SQLGenMonad a = StateT [String] IO a

sqlDialect :: Dialect
sqlDialect = ansi2011

addStatements :: [String] -> SQLGenMonad ()
addStatements [] = return ()
addStatements ss = do
  let txt = unlines ss
  case parseStatements sqlDialect "" Nothing $ unlines ss of
    Left err -> error $ "internal error: statements parsing failure: " ++ show err++ "\nstatements:\n--------------\n" ++ unlines ss ++"--------------"
    Right _ -> modify $ \ssPrev  -> ssPrev ++ ss

normalizeStatementString :: String -> String
normalizeStatementString cs = reduceSpaces $ onlySpaces cs
  where
    onlySpaces = map (\c -> if c < ' ' then ' ' else c)
    reduceSpaces (' ':' ':cs) = reduceSpaces $ ' ':cs
    reduceSpaces (c:cs) = c : reduceSpaces cs
    reduceSpaces cs = cs

commandAction :: Command -> SQLGenMonad ()
commandAction (MandatorySystemTables userTables initialRequests) = do
  addStatements userTables
  addStatements $
    [ "-- special table - current height. keep it single-valued."
    , "CREATE TABLE height (height INTEGER);"
    , ""
    , "-- special table - allowed requests."
    , "CREATE TABLE allowed_requests"
    , "    ( request_id STRING PRIMARY KEY -- we expect hash here, for now any string would do."
    , "    , request_text STRING"
    , "    );"
    , ""
    , "-- special table - parameters for requests."
    , "CREATE TABLE allowed_requests_params"
    , "    ( request_id STRING"
    , "    , request_param_name STRING"
    , "    , request_param_type STRING"
    , "    , CONSTRAINT unique_param_for_request UNIQUE (request_id, request_param_name)"
    , "    , CONSTRAINT request_must_exist FOREIGN KEY (request_id) REFERENCES allowed_requests(request_id)"
    , "    , CONSTRAINT correct_type CHECK (request_param_type = 'S' OR request_param_type = 'I' OR request_param_type = 'P')"
    , "    );"
    ]
  addStatements initialRequests
  addStatements
    [ "-- special table that keeps genesis requests"
    , "CREATE TABLE serialized_genesis_requests"
    , "    ( seq_index   INTEGER"
    , "    , request_sql STRING"
    , "    );"
    , ""
    , "-- special table that keeps serialized requests"
    , "CREATE TABLE serialized_requests"
    , "    ( height     INTEGER -- height for request"
    , "    , seq_index  INTEGER -- order inside the height"
    , "    , request_id STRING  -- the request id"
    , "    , CONSTRAINT serialized_requests_primary_key PRIMARY KEY (height, seq_index)"
    , "    , CONSTRAINT must_have_request_id FOREIGN KEY (request_id) REFERENCES allowed_requests(request_id)"
    , "    );"
    , ""
    , "-- special table that keeps actual parameter values for seriqlized requests"
    , "CREATE TABLE serialized_requests_values"
    , "    ( height     INTEGER -- height for request"
    , "    , seq_index  INTEGER -- order inside the height"
    , "    , request_id STRING  -- the text itself. must be request_id"
    , "    , request_param_name  STRING -- name of the parameter"
    , "    , request_param_value STRING -- and value of the parameter, interpreted according to the type from allowed_requests_params table"
    , "    , CONSTRAINT serialized_requests_values_primary_key PRIMARY KEY (height, seq_index, request_param_name)"
    , "    , CONSTRAINT must_have_request FOREIGN KEY (height, seq_index, request_id) REFERENCES serialized_requests(height, seq_index, request_id)"
    , "    );"
    ]
  ss <- get
  case parseStatements sqlDialect "" Nothing $ unlines ss of
    Left err -> error $ "internal error: error parsing combined statements: "++show err
    Right statements -> do
      let printedBack = map normalizeStatementString $
                            map (prettyStatement sqlDialect) statements
      addStatements
        [ "INSERT INTO serialized_genesis_requests " ++
          "(request_sql, seq_index) VALUES ("++sqlStr req++", "++show seqIndex++");"
        | (req, seqIndex) <- zip printedBack [0..]
        ]
commandAction (AddRequestCode req id params) = do
  let sqlId = sqlStr id
      sqlReq = sqlStr $ normalizeStatementString req
  addStatements $
    [ "-- adding a request"
    , "INSERT INTO allowed_requests (request_id, request_text) VALUES ("++sqlReq++", "++sqlId++");"
    , ""
    , "-- adding request parameters:"
    ] ++
    [ sqlText
    | (ptype, pname) <- params
    , let ptypeStr = case ptype of
                       IntParam -> "I"
                       StringParam -> "S"
                       PositiveParam -> "P"
    , let sqlText = unlines
            [ "INSERT INTO allowed_requests_params"
            , "    (request_id, request_param_type, request_param_name)"
            , "  VALUES"
            , "    ("++sqlId++", "++sqlStr ptypeStr++", "++sqlStr pname++");"
            ]
    ]

main :: IO ()
main = do
  cmd <- execParser (info (parseCommand <**> helper) idm)
  (>>= mapM_ putStrLn) $ flip execStateT [] $ commandAction cmd

data PType = IntParam | StringParam | PositiveParam
data Command =
    MandatorySystemTables [String] [String]
  | AddRequestCode String String [(PType, String)]

parseCommand :: Parser Command
parseCommand = subparser
  $  command "mandatory-system-tables" (info (MandatorySystemTables <$> many (sqlOption "table") <*> many (sqlOption "request")) idm)
  <> command "add-request-code" (info (AddRequestCode <$> requestText <*> requestId <*> some typedParam) idm)
  where
    requestText = strOption (long "request")
    requestId = strOption (long "id")
    typedParam :: Parser (PType, String)
    typedParam =
          ((,) IntParam <$> strOption (long "int"))
      <|> ((,) StringParam <$> strOption (long "string"))
      <|> ((,) PositiveParam <$> strOption (long "positive"))
    sqlOption opt = option (eitherReader tryToParseSQL) (long opt)
    tryToParseSQL s = either (Left . (++s) . (++ "\n--------------\n") . show)
                        (Right . const s) $
                        parseStatements sqlDialect "" Nothing s
