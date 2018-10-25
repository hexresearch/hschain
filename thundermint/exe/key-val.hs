
import Control.Monad
import Data.Monoid         ((<>))
import Options.Applicative

import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as BC8

import Thundermint.Crypto.Ed25519
import Thundermint.Mock.KeyVal
import Thundermint.Store
import Thundermint.Run


main :: IO ()
main
  = join . customExecParser (prefs showHelpOnError)
  $ info (helper <*> parser)
    (  fullDesc
    <> header   "Coin test program"
    <> progDesc ""
    )
  where
    work maxH prefix file = do
      -- Run blockchain
      blob <- BC8.readFile file
      spec <- case JSON.eitherDecodeStrict blob of
        Right s -> return s
        Left  e -> error e
      storageList <- executeSpec maxH prefix spec
      -- Check result against consistency invariants
      let storage :: BlockStorage Ed25519_SHA512 [(String,Int)]
          storage = blockStorage
      checks <- forM storageList $ \c -> runDBT c $ checkStorage storage
      unless (null (concat checks)) $ error $ "Consistency problem: " ++ (show checks)
    ----------------------------------------
    parser :: Parser (IO ())
    parser
      = pure work
     <*> optional (option auto
                    (  long    "max-h"
                    <> metavar "N"
                    <> help    "Maximum height"
                    ))
     <*> option str
          (  long    "prefix"
          <> value   "."
          <> metavar "PATH"
          <> help    "prefix for db & logs"
          )
     <*> argument str
         (  help "Specification file"
         <> metavar "JSON"
         )
