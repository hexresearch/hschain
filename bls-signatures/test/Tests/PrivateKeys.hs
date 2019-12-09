{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.PrivateKeys (testBls) where

import Test.Tasty
import Test.Tasty.HUnit

import Crypto.Bls as Bls


testBls :: TestTree
testBls = testGroup "Private keys" [
    testCase "1" $ do
        -- Note: all initial seeds, message and expected values got from original BLS
        let seed1 = "\1\2\3\4\5"
            seed2 = "\1\2\3\4\5\6"
            message1 = "\7\8\9"
            sk1 = fromSeed seed1
            sk2 = fromSeed seed2
        (hexify $ serializePrivateKey sk1) @?= "022fb42c08c12de3a6af053880199806532e79515f94e83461612101f9412f9e"
        (hexify $ serializePrivateKey sk2) @?= "502c5661f5af46ed48ddc3b5332e21b93cc7d0a84df46c4b9c7fe8f25ef48d66"
        let pk1 = getPublicKey sk1
        let pk2 = getPublicKey sk2
        getFingerprint pk1 @?= 0x26d53247
        getFingerprint pk2 @?= 0x289bb56e
        (hexify $ serializePublicKey pk1) @?= "02a8d2aaa6a5e2e08d4b8d406aaf0121a2fc2088ed12431e6b0663028da9ac5922c9ea91cde7dd74b7d795580acc7a61"
        (hexify $ serializePublicKey pk2) @?= "83fbcbbfa6b7a5a0e707efaa9e6de258a79a59116dd889ce74f1ab7f54c9b7ba15439dcb4acfbdd8bcffdd8825795b90"
        let sig1 = signInsecure sk1 message1
            sig2 = signInsecure sk2 message1
        (hexify $ serializeInsecureSignature sig1) @?= "93eb2e1cb5efcfb31f2c08b235e8203a67265bc6a13d9f0ab77727293b74a357ff0459ac210dc851fcb8a60cb7d393a419915cfcf83908ddbeac32039aaa3e8fea82efcb3ba4f740f20c76df5e97109b57370ae32d9b70d256a98942e5806065"
        (hexify $ serializeInsecureSignature sig2) @?= "975b5daa64b915be19b5ac6d47bc1c2fc832d2fb8ca3e95c4805d8216f95cf2bdbb36cc23645f52040e381550727db420b523b57d494959e0e8c0c6060c46cf173872897f14d43b2ac2aec52fc7b46c02c5699ff7a10beba24d3ced4e89c821e"
    , testCase "hash" $ do
        (hexify $ hash256 "\1\2\3") @?= "039058c6f2c0cb492c533b0a4d14ef77cc0f78abccced5287d84a1a2011cfb81"
    , testCase "aggs" $ do
        let message = "My test message"
            msgHash = hash256 message
            sk1 = fromSeed "rnd1"
            sk2 = fromSeed "rnd2"
            sig1 = signInsecure sk1 message
            sig2 = signInsecure sk2 message
        -- TODO assert $ sig1 /= sig2
        assert $ insecureVerify sig1 [msgHash] [getPublicKey sk1]
        assert $ insecureVerify sig2 [msgHash] [getPublicKey sk2]
        let sigs = [sig1, sig2]
            pks =  [getPublicKey sk1, getPublicKey sk2]
            aggSig = aggregateInsecureSignatures sigs
            aggPk  = aggregateInsecurePublicKeys pks
        assert $ insecureVerify aggSig [msgHash] [aggPk]
    ]
