module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import qualified Data.List as List

import Data.String

import System.Exit

import Text.Printf

import Options.Applicative

import HSChain.POW

data PrintOpt = PrintText | PrintHex
  deriving (Show)

data Command =
    FindAnswer ByteString Integer PrintOpt
  | CheckBlock ByteString ByteString ByteString Integer
  deriving (Show)

parser :: Parser Command
parser = subparser
  (  command "find" (info findAnswerParser $ header "find an answer")
  <> command "check" (info checkAnswerParser $ header "checking an answer")
  )
  where
    findAnswerParser = FindAnswer
                     <$> parseByteString "source" <*> parseTarget <*> parsePrint
    checkAnswerParser = CheckBlock <$> parseByteString "answer"
                                   <*> parseByteString "source" <*> parseHash <*> parseTarget
    parsePrint = flag' PrintText (short 'T' <> long "print-as-text")
               <|> flag' PrintHex (short 'H' <> long "print-as-hex")
    parseByteString opt = parseAsText opt <|> parseAsHex opt
    parseAsText opt = option (maybeReader bytestringFromString) (long $ opt ++ "-text")
    parseAsHex opt = option (maybeReader bytestringFromHex) (long $ opt ++ "-hex")
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
    parseTarget = option (maybeReader parseInt) (long "target" <> short 't')
    parseInt :: (Num a, Read a) => String -> Maybe a
    parseInt s = case reads s of
                   ((i,""):_) -> Just i
                   _ -> Nothing

main :: IO ()
main = do
  cmd <- execParser $ info parser mempty
  case cmd of
    FindAnswer prefix target printOpt -> do
      let config = defaultPOWConfig
                 { powCfgTarget = target
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
    CheckBlock block answer hash target -> do
      r <- check block answer hash $ defaultPOWConfig
                                     { powCfgTarget = target
                                     }
      putStrLn $ "check result: "++show r

