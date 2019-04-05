{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Monad.IO.Class
import Codec.CBOR.FlatTerm (FlatTerm)
import Data.Foldable
import Data.List
import Options.Applicative

import Thundermint.Types
import Thundermint.Store
import Thundermint.Crypto.Ed25519

----------------------------------------------------------------
-- Reader for database
----------------------------------------------------------------

type Alg = Ed25519_SHA512


printHeight :: DBT 'RO Alg FlatTerm IO ()
printHeight =
  liftIO . print =<< queryRO blockchainHeight

printValidators :: DBT 'RO Alg FlatTerm IO ()
printValidators = do
  hBch <- queryRO blockchainHeight
  for_ [Height 1 .. succ hBch] $ \h -> do
    liftIO $ putStrLn $ "==== " ++ show h
    queryRO (retrieveValidatorSet h) >>= \case
      Nothing -> liftIO $ putStrLn "ERROR: no validator set"
      Just v  -> liftIO $ mapM_ print $ sort $ asValidatorList v
    
runCommand :: FilePath -> DBT 'RO Alg FlatTerm IO () -> IO ()
runCommand path dbt =
  withConnection path $ \c -> runDBT c dbt


----------------------------------------------------------------
--
----------------------------------------------------------------

main :: IO ()
main
  = join . customExecParser (prefs showHelpOnError)
  $ info (helper <*> parser)
    (  fullDesc
    <> header   "Thundermint DB reader program"
    <> progDesc ""
    )
  where
    parser :: Parser (IO ())
    parser = do
      db <- argument str
        (  help "Database"
        <> metavar "DB"
        )
      cmd <- dbCommand
      return $ runCommand db cmd


dbCommand :: Parser (DBT 'RO Alg FlatTerm IO ())
dbCommand = subparser $ mconcat
  [ command "height"
  $ info (helper <*> pure printHeight)
         (progDesc "Display height of database")
  , command "validators"
  $ info (helper <*> pure printValidators)
         (progDesc "Display validator sets for each heigt")
  ]
