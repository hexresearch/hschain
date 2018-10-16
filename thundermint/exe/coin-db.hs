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

import Thundermint.Blockchain.Types
import Thundermint.Store
import Thundermint.Store.SQLite
import Thundermint.Mock.Coin

----------------------------------------------------------------
-- Reader for database
----------------------------------------------------------------

totalNOfBlocks :: BlockStorage 'RO IO Alg [Tx] -> IO ()
totalNOfBlocks storage@BlockStorage{..} = do
  blocks <- loadAllBlocks storage
  print $ length $ blockData =<< blocks

printHeight :: BlockStorage 'RO IO Alg [Tx] -> IO ()
printHeight BlockStorage{..} =
  print =<< blockchainHeight


loadAllBlocks :: Monad m => BlockStorage ro m alg a -> m [Block alg a]
loadAllBlocks storage = go (Height 0)
  where
    go h = retrieveBlock storage h >>= \case
      Nothing -> return []
      Just b  -> (b :) <$> go (succ h)

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



dbCommand :: Parser (BlockStorage 'RO IO Alg [Tx] -> IO ())
dbCommand =
  argument (maybeReader $ \case
               "tr_total" -> Just totalNOfBlocks
               "height" -> Just printHeight
               _          -> Nothing
           )
     ( help "N of transactions" <> metavar "COMMADN" )

