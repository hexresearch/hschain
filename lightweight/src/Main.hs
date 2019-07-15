module Main where

import qualified LTM.SimpleDemo as SD
import qualified LTM.KeyValueDemo as KVD

main :: IO ()
main = do
  putStrLn "Simple demo, 200 messages:" >> SD.demo 200
  putStrLn "Key-value store demo, 200 messages:" >> KVD.demo 400
