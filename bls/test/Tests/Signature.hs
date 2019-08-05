{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Signature (htf_thisModulesTests) where

import Test.Framework

import Crypto.Bls

import Data.ByteString.Builder as BS
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL


hexifyBs :: BS.ByteString -> BS.ByteString
hexifyBs = BSL.toStrict . BS.toLazyByteString . BS.byteStringHex


test_pk :: IO ()
test_pk = do
    -- putStrLn "BEFORE"
    sk <- fromSeed "abcdef"
    -- bs <- serialize sk -- TODO сравнить с результатом
    -- "4c4fa389df7397c8b899dc32920ab92f2fd450b2935a5276101b13c5721a06d3"
    -- BS.putStrLn (hexifyBs bs)
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
