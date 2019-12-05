module Main where

import Control.Monad
import Control.Monad.State

import System.Exit (exitSuccess, exitFailure)

import Options.Applicative

import HSChain.SQL

walletDemoTableName :: String
walletDemoTableName = "wallet"

commandAction :: Command -> SQLGenMonad ()
commandAction (MandatorySystemTables userTables initialRequests keyRoles) = do
  addStatements userTables
  addStatements $
    [ "-- special table - current height. keep it single-valued."
    , "CREATE TABLE height (height INTEGER);"
    , "INSERT INTO height (height) VALUES (-1); -- not even genesis was read"
    , ""
    , "-- special table - allowed requests."
    , "CREATE TABLE allowed_requests"
    , "    ( request_id TEXT PRIMARY KEY -- we expect hash here, for now any string would do."
    , "    , request_text TEXT"
    , "    , CONSTRAINT non_empty_request_id CHECK (length(request_id) > 0)"
    , "    , CONSTRAINT non_empty_request_text CHECK (length(request_text) > 0)"
    , "    );"
    , ""
    , "-- special table - parameters for requests."
    , "CREATE TABLE allowed_requests_params"
    , "    ( request_id TEXT"
    , "    , request_param_name TEXT"
    , "    , request_param_type TEXT"
    , "    , CONSTRAINT unique_param_for_request UNIQUE (request_id, request_param_name)"
    , "    , CONSTRAINT request_must_exist FOREIGN KEY (request_id) REFERENCES allowed_requests(request_id)"
    , "    , CONSTRAINT correct_type CHECK (request_param_type = 'S' OR request_param_type = 'I' OR request_param_type = 'P')"
    , "    );"
    ]
  addStatements initialRequests
  addStatements
    [ "-- special table that keeps serialized requests"
    , "CREATE TABLE serialized_requests"
    , "    ( height     INTEGER -- height for request"
    , "    , seq_index  INTEGER -- order inside the height"
    , "    , request_id TEXT  -- the request id"
    , "    , CONSTRAINT serialized_requests_primary_key PRIMARY KEY (height, seq_index)"
    , "    , CONSTRAINT must_have_request_id FOREIGN KEY (request_id) REFERENCES allowed_requests(request_id)"
    , "    );"
    , ""
    , "-- special table that keeps actual parameter values for seriqlized requests"
    , "CREATE TABLE serialized_requests_params"
    , "    ( height     INTEGER -- height for request"
    , "    , seq_index  INTEGER -- order inside the height"
    , "    , request_id TEXT  -- the text itself. must be request_id"
    , "    , request_param_name  TEXT -- name of the parameter"
    , "    , request_param_value TEXT -- and value of the parameter, interpreted according to the type from allowed_requests_params table"
    , "    , CONSTRAINT serialized_requests_values_primary_key PRIMARY KEY (height, seq_index, request_param_name)"
    , "    , CONSTRAINT must_have_request FOREIGN KEY (height, seq_index, request_id) REFERENCES serialized_requests(height, seq_index, request_id)"
    , "    );"
    ]
  addStatements
    [ "-- special table of roles of public keys"
    , "CREATE TABLE public_key_role"
    , "    ( public_key    TEXT NOT NULL"
    , "    , role          TEXT NOT NULL"
    , "    , CONSTRAINT role_of_key PRIMARY KEY (public_key, role)"
    , "    );"
    ]
  forM_ keyRoles $ \(key, role) -> do
    addStatements
      [ "INSERT INTO public_key_role (public_key, role) VALUES ("++sqlStr key++", "++sqlStr role++");"]
  ss <- get -- we do not need our internal tables exposed to the client.
  addStatements
    [ "-- special table that keeps genesis requests"
    , "CREATE TABLE serialized_genesis_requests"
    , "    ( seq_index   INTEGER"
    , "    , request_sql TEXT"
    , "    );"
    , ""
    ]
  case parseSQLStatements $ unlines ss of
    Left err -> error $ "internal error: error parsing combined statements: "++show err
    Right statements -> do
      let printedBack = map normalizeStatementString $
                            map prettySQLStatement statements
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
    , "INSERT INTO allowed_requests (request_text, request_id) VALUES ("++sqlReq++", "++sqlId++");"
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
    MandatorySystemTables [String] [String] [(String, String)]
  | AddRequestCode String String [(PType, String)]

parseCommand :: Parser Command
parseCommand = subparser
  $  command "mandatory-system-tables"
    (info (MandatorySystemTables
        <$> many (sqlOption "table")
        <*> many (sqlOption "request")
        <*> many (option (eitherReader keyRoleParser) (long "key-role"))) idm)
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
                        parseSQLStatements s
    keyRoleParser s
        | length key < 1 = Left "format of key-role argument must be PUBLICKEY:ROLE"
        | length role < 1 = Left "role must not be empty"
        | otherwise = Right (key, role)
      where
        (key, role) = span (==':') s

