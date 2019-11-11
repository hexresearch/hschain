{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
import Control.Monad
import Control.Monad.IO.Class
import Codec.CBOR.FlatTerm (FlatTerm)
import Data.Foldable
import Data.List
import Options.Applicative

import HSChain.Types
import HSChain.Store
import HSChain.Crypto
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA

----------------------------------------------------------------
-- Reader for database
----------------------------------------------------------------

type Alg = Ed25519 :& SHA512


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
    <> header   "HSChain DB reader program"
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
