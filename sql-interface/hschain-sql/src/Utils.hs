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

main :: IO ()
main = do
  cmd <- execParser (info (parseCommand <**> helper) idm)
  case cmd of
    MandatorySystemTables ->
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
    AddRequestCode req id params -> do
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

data PType = IntParam | StringParam | PositiveParam
data Command =
    MandatorySystemTables
  | AddRequestCode String String [(PType, String)]

parseCommand :: Parser Command
parseCommand = subparser
  $  command "mandatory-system-tables" (info (pure MandatorySystemTables) idm)
  <> command "add-request-code" (info (AddRequestCode <$> requestText <*> requestId <*> some typedParam) idm)
  where
    requestText = strOption (long "request")
    requestId = strOption (long "id")
    typedParam :: Parser (PType, String)
    typedParam =
          ((,) IntParam <$> strOption (long "int"))
      <|> ((,) StringParam <$> strOption (long "string"))
      <|> ((,) PositiveParam <$> strOption (long "positive"))
