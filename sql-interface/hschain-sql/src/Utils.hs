module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)

usage :: Maybe String -> IO ()
usage mbStr = do
  putStrLn "usage: hschain-sql COMMAND ARGS"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  help - gives this text message"
  putStrLn "  genesis-sql - dumps CREATE TABLE and INSERT statements needed to create the database"
  maybe (return ()) (putStrLn . ("\nerror: "++)) mbStr
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case (if null args then ["help"] else args) of
    ("help":_) -> usage Nothing
    _ -> usage (Just "invalid command line")
