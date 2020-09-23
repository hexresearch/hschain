{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
import Control.Monad
import qualified Data.Aeson as JSON
import Data.Maybe
import Data.Yaml (decodeFileThrow)
import Data.Map  (Map,(!))
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Options.Applicative

import HSChain.Crypto
import HSChain.Examples.Coin (Alg,signTX)

main :: IO ()
main = do
  act <- customExecParser (prefs showHelpOnError)
       $ info (helper <*> parser)
         (  fullDesc
         <> header   "Program for working with keys for coin node"
         <> progDesc ""
         )
  act


parser :: Parser (IO ())
parser = subparser
  ( command "gen"  (parserGen  `info` header "Generate key(s)")
 <> command "pk"   (parserPK   `info` header "Compute public key")
 <> command "sign" (parserSign `info` header "Sign transaction")
  )

-- Sign transaction
parserSign :: Parser (IO ())
parserSign = helper <*> do
  keyring <- strOption ( short 'k'
                      <> long  "keyring"
                      <> help  "Keyring file"
                      <> metavar "FILE"
                       )
  key  <- strArgument ( help    "key handle"
                     <> metavar "KEY"
                      )
  path <- strArgument ( help    "File with TX"
                     <> metavar "PATH"
                      )
  pure $ do
    keymap :: Map String T.Text <- decodeFileThrow keyring
    let Just (sk :: PrivKey Alg) = decodeBase58 $ keymap ! key
    mtx <- case path of
            "-" -> JSON.decode <$> BL8.getContents
            _   -> JSON.decodeFileStrict path
    case mtx of
      Nothing -> error "Cannot decode transaction"
      Just tx -> BL8.putStrLn . JSON.encode . signTX sk $ tx


-- Print public key
parserPK :: Parser (IO ())
parserPK = helper <*> do
  keyring <- strOption ( short 'k'
                      <> long  "keyring"
                      <> help  "Keyring file"
                      <> metavar "FILE"
                       )
  keyList <- many
           $ strArgument ( help    "List of key handles"
                        <> metavar "KEY"
                         )
  pure $ do
    keymap :: Map String T.Text <- decodeFileThrow keyring
    forM_ keyList $ \k -> do
      case decodeBase58 =<< Map.lookup k keymap of
        Nothing -> return ()
        Just sk -> putStrLn $ T.unpack $ encodeBase58 $ publicKey @Alg sk

-- Generate fresh private keys
parserGen :: Parser (IO ())
parserGen = helper <*> do
  n <- fmap (fromMaybe 1)
     $ optional
     $ argument auto
     $ ( help "Number of keys"
      <> metavar "N" 
       )
  pure $ act n
  where
    act n = replicateM_ n $ do
      putStrLn . T.unpack . encodeBase58 =<< generatePrivKey @Alg

