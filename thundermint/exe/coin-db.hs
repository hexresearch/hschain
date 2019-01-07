{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
import Control.Monad
import Data.Monoid
import Options.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as B

import Thundermint.Types.Blockchain
import Thundermint.Store
import Thundermint.Mock.Coin

----------------------------------------------------------------
-- Reader for database
----------------------------------------------------------------

totalNOfBlocks :: DBT 'RO Alg [Tx] IO ()
totalNOfBlocks = do
  blocks <- loadAllBlocks
  lift $ B.putStrLn $ JSON.encode $ fmap blockData blocks

printHeight :: DBT 'RO Alg [Tx] IO ()
printHeight = do
  height <- queryRO $  blockchainHeight
  lift $ print height


loadAllBlocks :: DBT 'RO Alg [Tx] IO [Block Alg [Tx]]
loadAllBlocks = go (Height 0)
  where
    go h = queryRO (retrieveBlock h) >>= \case
            Nothing -> return []
            Just b  -> (b :) <$> go (succ h)

withSQLiteBlockStorageRO :: MonadIO m
                         => FilePath -> DBT 'RO alg a m b -> m b
withSQLiteBlockStorageRO db command' = do
  conn <- connectionRO <$> openConnection db
  runDBT conn $ command'
----------------------------------------------------------------
--
----------------------------------------------------------------

main :: IO ()
main
  = join . customExecParser (prefs showHelpOnError)
  $ info (helper <*> parser)
    (  fullDesc
    <> header   "Coin DB reader program"
    <> progDesc ""
    )
  where
    work command' db = withSQLiteBlockStorageRO db command'
    parser :: Parser (IO ())
    parser
      = pure work
     <*> dbCommand 
     <*> argument str
         (  help "Database"
         <> metavar "DB"
         )



dbCommand :: Parser (DBT 'RO Alg [Tx] IO ())
dbCommand =
  argument (maybeReader $ \case
               "tr_total" -> Just totalNOfBlocks
               "height" -> Just printHeight
               _          -> Nothing
           )
     ( help "N of transactions" <> metavar "COMMADN" )

