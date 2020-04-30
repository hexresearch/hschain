module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import qualified Data.List as List

import Data.String
import Data.Word

import System.Exit

import Text.Printf

import Options.Applicative

import HSChain.POW

data PrintOpt = PrintText | PrintHex
  deriving (Show)

data Command =
    FindAnswer ByteString Int Word16 PrintOpt
  | CheckBlock ByteString ByteString Int Word16
  deriving (Show)

parser :: Parser Command
parser = subparser
  (  command "find" (info findAnswerParser $ header "find an answer")
  <> command "check" (info checkAnswerParser $ header "checking an answer")
  )
  where
    findAnswerParser = FindAnswer
                     <$> parseByteString <*> parseShift <*> parseMantissa <*> parsePrint
    checkAnswerParser = CheckBlock <$> parseByteString <*> parseHash <*> parseShift <*> parseMantissa
    parsePrint = flag' PrintText (short 'T' <> long "print-as-text")
               <|> flag' PrintHex (short 'H' <> long "print-as-hex")
    parseByteString = parseAsText <|> parseAsHex
    parseAsText = option (maybeReader bytestringFromString) (long "prefix-text")
    parseAsHex = option (maybeReader bytestringFromHex) (long "prefix-hex")
    parseHash = option (maybeReader bytestringFromHex) (long "hash-hex")
    bytestringFromString s = case reads s of
                               ((bs,""):_) -> Just $ fromString bs
                               _ -> Nothing
    bytestringFromHex s = fmap B.pack $ rd (words s)
      where
        rd [] = Just []
        rd (w:ws) = case reads $ "0x"++w of
          ((byte, ""):_) -> fmap (byte:) $ rd ws
          _ -> Nothing
    parseShift = option (maybeReader parseInt) (long "shift" <> short 's')
    parseInt :: (Num a, Read a) => String -> Maybe a
    parseInt s = case reads s of
                   ((i,""):_) -> Just i
                   _ -> Nothing
    parseMantissa = option (maybeReader parseInt) (long "mantissa" <> short 'm')

main :: IO ()
main = do
  cmd <- execParser $ info parser mempty
  case cmd of
    FindAnswer prefix shift mantissa printOpt -> do
      let config = defaultPOWConfig
                 { powCfgComplexityShift     = shift
                 , powCfgComplexityMantissa  = mantissa
                 }
      r <- solve [prefix] config
      case r of
        Nothing -> putStrLn "failed to find answer" >> exitFailure
        Just (answer, hash) -> case printOpt of
          PrintText -> do
            putStrLn $ "complete header: "++show completeBlock
          PrintHex -> do
            putStrLn $ "complete header: "++
                     List.intercalate " "
                          (map (printf "%02x") $ B.unpack completeBlock)
            putStrLn $ "complete header hash: "++
                     List.intercalate " "
                          (map (printf "%02x") $ B.unpack hash)
          where
            completeBlock = B.concat [prefix, answer]
    CheckBlock withAnswer hash shift mantissa -> do
      r <- check withAnswer hash $ defaultPOWConfig
                                     { powCfgComplexityShift = shift
                                     , powCfgComplexityMantissa = mantissa
                                     }
      putStrLn $ "check result: "++show r

