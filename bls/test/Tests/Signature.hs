{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Signature (htf_thisModulesTests) where

import Test.Framework

import Crypto.Bls


test_some :: IO ()
test_some = do
    assertEqual 3 (someFunc 1 2)

test_pk :: IO ()
test_pk = do
    -- putStrLn "BEFORE"
    sk <- fromSeed "abcdef"
    pk <- getPublicKey sk
    let message1 = "my looooong message"
    let message2 = "mY looooong message"
    sig' <- sign sk message1
    ai <- fromMsg pk message2
    sig <- setAggregationInfo sig' ai
    r <- verify sig
    -- putStrLn "END"
    print r
    return ()
