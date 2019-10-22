{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Signature (htf_thisModulesTests) where

import Test.Framework
import qualified Data.Vector as V

import Crypto.Bls


-- hexifyBs :: BS.ByteString -> BS.ByteString
-- hexifyBs = BSL.toStrict . BS.toLazyByteString . BS.byteStringHex


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


test_nofn :: IO ()
test_nofn = do
    let message = "Some test message"
    sk1 <- fromSeed "abc"
    sk2 <- fromSeed "def"
    pk1 <- getPublicKey sk1
    pk2 <- getPublicKey sk2
    messageHash <- hash256 message
    sig1 <- signInsecure sk1 message
    sig2 <- signInsecure sk2 message
    assertBool =<< verifyInsecure sig1 (V.singleton messageHash) (V.singleton pk1)
    assertBool =<< verifyInsecure sig2 (V.singleton messageHash) (V.singleton pk2)
    aggSig <- aggregateInsecureSignatures (V.fromList [sig1, sig2])
    aggPk  <- aggregateInsecurePublicKey  (V.fromList [pk1,  pk2])
    assertBool =<< verifyInsecure aggSig (V.singleton messageHash) (V.singleton aggPk)


test_nofnPrehashed :: IO ()
test_nofnPrehashed = do
    let message = "Some test message"
    sk1 <- fromSeed "abc"
    sk2 <- fromSeed "def"
    pk1 <- getPublicKey sk1
    pk2 <- getPublicKey sk2
    messageHash <- hash256 message
    sig1 <- signInsecurePrehashed sk1 messageHash
    sig2 <- signInsecurePrehashed sk2 messageHash
    assertBool =<< verifyInsecure sig1 (V.singleton messageHash) (V.singleton pk1)
    assertBool =<< verifyInsecure sig2 (V.singleton messageHash) (V.singleton pk2)
    aggSig <- aggregateInsecureSignatures (V.fromList [sig1, sig2])
    aggPk  <- aggregateInsecurePublicKey  (V.fromList [pk1,  pk2])
    assertBool =<< verifyInsecure aggSig (V.singleton messageHash) (V.singleton aggPk)



test_nofn1 :: IO ()
test_nofn1 = do
    let message = "Some test message"
    sk1 <- fromSeed "abc"
    sk2 <- fromSeed "def"
    pk1 <- getPublicKey sk1
    pk2 <- getPublicKey sk2
    messageHash <- hash256 message
    sig1 <- signInsecurePrehashed sk1 messageHash
    sig2 <- signInsecurePrehashed sk2 messageHash
    assertBool =<< verifyInsecure1 sig1 messageHash pk1
    assertBool =<< verifyInsecure1 sig2 messageHash pk2
    aggSig <- aggregateInsecureSignatures (V.fromList [sig1, sig2])
    aggPk  <- aggregateInsecurePublicKey  (V.fromList [pk1,  pk2])
    assertBool =<< verifyInsecure1 aggSig messageHash aggPk

