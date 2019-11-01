module Main where

import System.Exit (exitSuccess, exitFailure)

import Language.SQL.SimpleSQL.Dialect
import Language.SQL.SimpleSQL.Parse

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

printWalletDemoTables :: IO ()
printWalletDemoTables = do
  putStrLn $ unlines $
    [ "-- funds available"
    , "CREATE TABLE "++walletDemoTableName
    , "  ( wallet_id STRING PRIMARY KEY"
    , "  , amount    INTEGER"
    , "  , CONSTRAINT amount >= 0"
    , "  );"
    ] ++
    [ unwords
        [ "INSERT INTO "++walletDemoTableName
        , "(wallet_id, amount)"
        , "VALUES ("++sqlStr id++", "++show amount++");"
        ]
    | (id, amount) <- map (flip (,) 1000000) ["u1", "u2", "u3", "u4"]
    ]

commandAction :: Command -> IO ()
commandAction MandatorySystemTables =
  putStrLn $ unlines
    [ "-- special table - allowed requests."
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
    , "    , CONSTRAINT UNIQUE (request_id, request_param_name)"
    , "    , CONSTRAINT FOREIGN KEY (request_id) REFERENCES allowed_requests(request_id)"
    , "    , CONSTRAINT CHECK (request_param_type = \"S\" OR request_param_type = \"I\" OR request_param_type = \"P\")"
    , "    );"
    ]
commandAction (AddRequestCode req id params) = do
  let sqlId = sqlStr id
      sqlReq = sqlStr req
  putStrLn $ unlines $
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
commandAction WalletDemo = do
  printWalletDemoTables
  commandAction MandatorySystemTables
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

main :: IO ()
main = do
  cmd <- execParser (info (parseCommand <**> helper) idm)
  commandAction cmd

data PType = IntParam | StringParam | PositiveParam
data Command =
    MandatorySystemTables
  | AddRequestCode String String [(PType, String)]
  | WalletDemo

parseCommand :: Parser Command
parseCommand = subparser
  $  command "mandatory-system-tables" (info (pure MandatorySystemTables) idm)
  <> command "add-request-code" (info (AddRequestCode <$> requestText <*> requestId <*> some typedParam) idm)
  <> command "wallet-demo" (info (pure WalletDemo) idm)
  where
    requestText = strOption (long "request")
    requestId = strOption (long "id")
    typedParam :: Parser (PType, String)
    typedParam =
          ((,) IntParam <$> strOption (long "int"))
      <|> ((,) StringParam <$> strOption (long "string"))
      <|> ((,) PositiveParam <$> strOption (long "positive"))
