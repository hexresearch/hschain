{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
import Control.Monad
import Options.Applicative

import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as BC8

import Thundermint.Types.Blockchain
import Thundermint.Run
import Thundermint.Mock.Coin
import Thundermint.Store
import Thundermint.Store.SQL

import Data.Monoid ((<>))


----------------------------------------------------------------
--
----------------------------------------------------------------

main :: IO ()
main
  = join . customExecParser (prefs showHelpOnError)
  $ info (helper <*> parser)
    (  fullDesc
    <> header   "Coin test program"
    <> progDesc ""
    )
  where
    work maxH delay doValidate file = do
      blob <- BC8.readFile file
      spec <- case JSON.eitherDecodeStrict blob of
        Right s -> return s
        Left  e -> error e
      storageList <- executeNodeSpec maxH delay spec

      when doValidate $ do
        -- If maxH is nothing code is not reachable
        let Just maximumH = maxH
            heights = map (Height . fromIntegral) [0 .. maximumH]
            allEqual []     = error "Empty list impossible!"
            allEqual (x:xs) = all (x==) xs

        forM_ storageList $ \c -> runDBT c checkStorage

        forM_ heights $ \h -> do
          -- Check that all blocks match!
          blocks <- forM storageList $ \c -> do
            mb <- runDBT c $ queryRO $ retrieveBlock h
            case mb of
              Nothing -> error ("Missing block at " <> show h)
              Just b  -> return b
          when (not $ allEqual blocks) $
            error ("Block mismatch!" <> show h <> "\n" <> show blocks)
          -- Check that validator set match
          vals <- forM storageList $ \c -> do
            mv <- runDBT c $ queryRO $ retrieveValidatorSet h
            case (h,mv) of
              (Height 0, Nothing) -> return Nothing
              (_       , Just v ) -> return (Just v)
              _                   -> error "Invalid validator!"
          when (not $ allEqual vals) $
            error ("Validators mismatch!" <> show h)
          --
          utxos <- forM storageList $ \c ->
            runDBT c $ queryRO $ queryUserState h coinDict $ materializePMap unspentOutputsLens
          print $ (sum . fmap snd) <$> utxos
    ----------------------------------------
    parser :: Parser (IO ())
    parser
      = pure work
     <*> optional (option auto
                    (  long    "max-h"
                    <> metavar "N"
                    <> help    "Maximum height"
                    ))
     <*> option auto
           (  long    "delay"
           <> metavar "N"
           <> help    "delay between transactions in ms"
           )
     <*> switch
           (  long "check-consensus"
           <> help "validate databases"
           )
     <*> argument str
         (  help "Specification file"
         <> metavar "JSON"
         )


----------------------------------------------------------------
-- Additional functions meant to be run from GHCi
----------------------------------------------------------------
{-
pprCoinState :: CoinState -> IO ()
pprCoinState (CoinState outs) = do
  let balances = Map.fromListWith (+)
        [ (address pk, i)
        | (pk,i) <- toList outs
        ]
  mapM_ print $ Map.toList balances
  putStrLn $ "Σ = " ++ show (sum balances)

printCoinStateUpdates :: FilePath -> IO ()
printCoinStateUpdates dbName =
  withSQLiteBlockStorageRO dbName $ \(storage :: BlockStorage 'RO IO Alg [Tx]) -> do
    let step coin h = do
          Just Block{..} <- retrieveBlock storage h
          let Just coin' = processBlock transitions h blockData coin
          putStrLn ("==== " ++ show h ++ "================")
          pprCoinState coin'
          return coin'
    Height hMax <- blockchainHeight storage
    _ <- foldM step CoinState{ unspentOutputs = Map.empty} [Height h | h <- [0 .. hMax]]
    return ()
-}
