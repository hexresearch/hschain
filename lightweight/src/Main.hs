module Main where

import qualified System.Clock as Clock

import qualified LTM.SimpleDemo as SD
import qualified LTM.KeyValueDemo as KVD

main :: IO ()
main = do
  start <- Clock.getTime Clock.Monotonic
  let nops = 1000000
  KVD.parallelPerformance 500 1000 nops
  end <- Clock.getTime Clock.Monotonic
  let dt = Clock.diffTimeSpec end start
      nanoseconds = Clock.toNanoSecs dt
      seconds = fromIntegral nanoseconds / 1e9
  putStrLn $ "adding and deleting "++show nops++" took "++show seconds++" seconds, "++show (fromIntegral nops / seconds)++" operations per second."
--  putStrLn "Key-value store demo, 200 messages:" >> KVD.demo 400 100
--  putStrLn "Simple demo, 200 messages:" >> SD.demo 200
