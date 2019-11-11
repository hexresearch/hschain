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
sqlStr = show . concatMap escape
  where
    escape '"' = "\\\""
    escape '\'' = "\\'"
    escape c = [c]

walletDemoTableName :: String
walletDemoTableName = "wallet"

type SQLGenMonad a = StateT [String] IO a

sqlDialect :: Dialect
sqlDialect = ansi2011

addStatements :: [String] -> SQLGenMonad ()
addStatements ss = do
  let txt = unlines ss
  case parseStatements sqlDialect "" Nothing txt of
    Left err -> error $ "internal error: statements parsing failure: " ++ show err
    Right _ -> modify $ \ssPrev  -> ssPrev ++ ss

normalizeStatementString :: String -> String
normalizeStatementString cs = reduceSpaces $ onlySpaces cs
  where
    onlySpaces = map (\c -> if c < ' ' then ' ' else c)
    reduceSpaces (' ':' ':cs) = reduceSpaces $ ' ':cs
    reduceSpaces (c:cs) = c : reduceSpaces cs
    reduceSpaces cs = cs

commandAction :: Command -> SQLGenMonad ()
commandAction (MandatorySystemTables userTables) =
  addStatements
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
    , "    , CONSTRAINT correct_type CHECK (request_param_type = \"S\" OR request_param_type = \"I\" OR request_param_type = \"P\")"
    , "    );"
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
            , "    ("++sqlId++", "++show ptypeStr++", "++show pname++");"
            ]
    ]
commandAction (WalletDemoTables serverSide) = do
  commandAction $ MandatorySystemTables $
    [ "-- funds available"
    , "CREATE TABLE "++walletDemoTableName
    , "  ( wallet_id STRING PRIMARY KEY"
    , "  , amount    INTEGER"
    , "  , CONSTRAINT no_overdraft CHECK (amount >= 0)"
    , "  );"
    ] ++
    [ unwords
        [ "INSERT INTO "++walletDemoTableName
        , "(wallet_id, amount)"
        , "VALUES ("++sqlStr id++", "++show amount++");"
        ]
    | (id, amount) <- map (flip (,) 1000000) ["u1", "u2", "u3", "u4"]
    ]

  let req = unwords
              [ "UPDATE "++walletDemoTableName
              , "SET amount = CASE"
              ,              "WHEN wallet_id = :user_id      THEN amount - :transfer_amount"
              ,              "WHEN address = :dest_user_id THEN amount - :transfer_amount"
              ,              "END"
              , "WHERE     (address = :user_id OR address = :dest_user_id)"
              , "          AND :transfer_amount > 0;"
              ]
  commandAction $ AddRequestCode req "transfer"
      [ (PositiveParam, "transfer_amount")
      , (StringParam,   "user_id")
      ]
  when serverSide $ do
    addStatements
      [ "-- special table that keeps genesis requests"
      , "CREATE TABLE serialized_genesis_requests"
      , "    ( order       INTEGER"
      , "    , request_sql STRING"
      , "    );"
      , ""
      , "-- special table that keeps serialized requests"
      , "CREATE TABLE serialized_requests"
      , "    ( height     INTEGER -- height for request"
      , "    , order      INTEGER -- order inside the height"
      , "    , request    STRING  -- the request. must be request_id for height >= 0 and SQK text for genesis."
      , "    , CONSTRAINT serialized_requests_primary_key PRIMARY KEY (height, order)"
      , "    , CONSTRAINT must_have_request_id FOREIGN KEY (request_id) REFERENCES allowed_requests(request_id)"
      , "    );"
      , ""
      , "-- special table that keeps actual parameter values for seriqlized requests"
      , "CREATE TABLE serialized_requests_values"
      , "    ( height     INTEGER -- height for request"
      , "    , order      INTEGER -- order inside the height"
      , "    , request_id STRING  -- the text itself. must be request_id"
      , "    , request_param_name  STRING -- name of the parameter"
      , "    , request_param_value STRING -- and value of the parameter, interpreted according to the type from allowed_requests_params table"
      , "    , CONSTRAINT serialized_requests_values_primary_key PRIMARY KEY (height, order, request_param_name)"
      , "    , CONSTRAINT must_have_request FOREIGN KEY (height, order, request_id) REFERENCES serialized_requests(height, order, request_id)"
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
              "(order, request_sql) VALUES ("++sqlStr req++", "++show order++");"
          | (req, order) <- zip printedBack [0..]
          ]

main :: IO ()
main = do
  cmd <- execParser (info (parseCommand <**> helper) idm)
  (>>= mapM_ putStrLn) $ flip execStateT [] $ commandAction cmd

data PType = IntParam | StringParam | PositiveParam
data Command =
    MandatorySystemTables [String]
  | AddRequestCode String String [(PType, String)]
  | WalletDemoTables { serverTables :: Bool }

parseCommand :: Parser Command
parseCommand = subparser
  $  command "mandatory-system-tables" (info (MandatorySystemTables <$> many sqlOption) idm)
  <> command "add-request-code" (info (AddRequestCode <$> requestText <*> requestId <*> some typedParam) idm)
  <> command "wallet-demo-tables" (info (WalletDemoTables <$> serverFlag) idm)
  where
    requestText = strOption (long "request")
    requestId = strOption (long "id")
    serverFlag = switch (long "server-side")
    typedParam :: Parser (PType, String)
    typedParam =
          ((,) IntParam <$> strOption (long "int"))
      <|> ((,) StringParam <$> strOption (long "string"))
      <|> ((,) PositiveParam <$> strOption (long "positive"))
    sqlOption = option (eitherReader tryToParseSQL) (long "sql")
    tryToParseSQL s = either (Left . show) (Right . const s) $
                        parseStatement sqlDialect "" Nothing s
