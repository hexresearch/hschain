{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Bls (testBls) where


import Data.Char
import Data.Word
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString         as BS
import Data.ByteString (ByteString)
import Test.Tasty
import Test.Tasty.HUnit

import Crypto.Bls as Bls


testBls :: TestTree
testBls = testGroup "Private keys" [
    testCase "Signing and serializing" $ do
        -- Note: all initial seeds, message and expected values got from original BLS
        let seed1 = "\1\2\3\4\5"
            seed2 = "\1\2\3\4\5\6"
            message1 = "\7\8\9"
            sk1 = privateKeyFromSeed seed1
            sk2 = privateKeyFromSeed seed2
        (hexify $ privateKeySerialize sk1) @?= "022fb42c08c12de3a6af053880199806532e79515f94e83461612101f9412f9e"
        (hexify $ privateKeySerialize sk2) @?= "502c5661f5af46ed48ddc3b5332e21b93cc7d0a84df46c4b9c7fe8f25ef48d66"
        let pk1 = privateKeyGetPublicKey sk1
        let pk2 = privateKeyGetPublicKey sk2
        publicKeyGetFingerprint pk1 @?= 0x26d53247
        publicKeyGetFingerprint pk2 @?= 0x289bb56e
        (hexify $ publicKeySerialize pk1) @?= "02a8d2aaa6a5e2e08d4b8d406aaf0121a2fc2088ed12431e6b0663028da9ac5922c9ea91cde7dd74b7d795580acc7a61"
        (hexify $ publicKeySerialize pk2) @?= "83fbcbbfa6b7a5a0e707efaa9e6de258a79a59116dd889ce74f1ab7f54c9b7ba15439dcb4acfbdd8bcffdd8825795b90"
        let sig1 = signInsecure sk1 message1
            sig2 = signInsecure sk2 message1
        (hexify $ insecureSignatureSerialize sig1) @?=
            "93eb2e1cb5efcfb31f2c08b235e8203a67265bc6a13d9f0ab77727293b74a357ff0459ac210dc851fcb8a60cb7d393a4\
            \19915cfcf83908ddbeac32039aaa3e8fea82efcb3ba4f740f20c76df5e97109b57370ae32d9b70d256a98942e5806065"
        (hexify $ insecureSignatureSerialize sig2) @?=
            "975b5daa64b915be19b5ac6d47bc1c2fc832d2fb8ca3e95c4805d8216f95cf2bdbb36cc23645f52040e381550727db42\
            \0b523b57d494959e0e8c0c6060c46cf173872897f14d43b2ac2aec52fc7b46c02c5699ff7a10beba24d3ced4e89c821e"
    , testCase "Deserialization" $ do
        let sk1 = privateKeyFromBytes (dehexify "022fb42c08c12de3a6af053880199806532e79515f94e83461612101f9412f9e")
            sk2 = privateKeyFromBytes (dehexify "502c5661f5af46ed48ddc3b5332e21b93cc7d0a84df46c4b9c7fe8f25ef48d66")
            message1 = "\7\8\9"
            pk1 = privateKeyGetPublicKey sk1
            pk2 = privateKeyGetPublicKey sk2
            sig1 = signInsecure sk1 message1
            sig2 = signInsecure sk2 message1
        publicKeyGetFingerprint pk1 @?= 0x26d53247
        publicKeyGetFingerprint pk2 @?= 0x289bb56e
        (hexify $ publicKeySerialize $ publicKeyFromBytes $ publicKeySerialize pk1) @?=
            "02a8d2aaa6a5e2e08d4b8d406aaf0121a2fc2088ed12431e6b0663028da9ac5922c9ea91cde7dd74b7d795580acc7a61"
        (hexify $ publicKeySerialize $ publicKeyFromBytes $ publicKeySerialize pk2) @?=
            "83fbcbbfa6b7a5a0e707efaa9e6de258a79a59116dd889ce74f1ab7f54c9b7ba15439dcb4acfbdd8bcffdd8825795b90"
        (hexify $ insecureSignatureSerialize $ insecureSignatureFromBytes $ insecureSignatureSerialize sig1) @?=
            "93eb2e1cb5efcfb31f2c08b235e8203a67265bc6a13d9f0ab77727293b74a357ff0459ac210dc851fcb8a60cb7d393a4\
            \19915cfcf83908ddbeac32039aaa3e8fea82efcb3ba4f740f20c76df5e97109b57370ae32d9b70d256a98942e5806065"
        (hexify $ insecureSignatureSerialize $ insecureSignatureFromBytes $insecureSignatureSerialize sig2) @?=
            "975b5daa64b915be19b5ac6d47bc1c2fc832d2fb8ca3e95c4805d8216f95cf2bdbb36cc23645f52040e381550727db42\
            \0b523b57d494959e0e8c0c6060c46cf173872897f14d43b2ac2aec52fc7b46c02c5699ff7a10beba24d3ced4e89c821e"
    , testCase "Equivalence" $ do
        let message = "Let's sign me!"
            sk1 = privateKeyFromSeed "\10\20\30\40\50"
            sk2 = privateKeyFromSeed "\60\70\80\90\10"
            pk1 = privateKeyGetPublicKey sk1
            pk2 = privateKeyGetPublicKey sk2
            sig1 = signInsecure sk1 message
            sig2 = signInsecure sk2 message
        (not $ sk1  `privateKeyEq` sk2) @? "Private keys must non be equals"
        (      sk1  `privateKeyEq` sk1) @? "Private keys must be equals"
        (      sk1  `privateKeyEq` (privateKeyFromBytes $ privateKeySerialize sk1)) @? "Private keys must be equals"
        (      sk2  `privateKeyEq` (privateKeyFromBytes $ privateKeySerialize sk2)) @? "Private keys must be equals"
        (not $ sig1 `insecureSignatureEq` sig2) @? "Signatures must non be equals"
        (      sig1 `insecureSignatureEq` sig1) @? "Signatures must be equals"
        (      sig2 `insecureSignatureEq` sig2) @? "Signatures must be equals"
        (      sig1 `insecureSignatureEq` (insecureSignatureFromBytes $ insecureSignatureSerialize sig1)) @? "Signatures must be equals"
        (      sig2 `insecureSignatureEq` (insecureSignatureFromBytes $ insecureSignatureSerialize sig2)) @? "Signatures must be equals"
        (not $ pk1 `publicKeyEq` pk2) @? "Public keys must non be equals"
        (      pk1 `publicKeyEq` pk1) @? "Public keys must be equals"
        (      pk2 `publicKeyEq` pk2) @? "Public keys must be equals"
        (      pk1 `publicKeyEq` (publicKeyFromBytes $ publicKeySerialize pk1)) @? "Public keys must be equals"
        (      pk2 `publicKeyEq` (publicKeyFromBytes $ publicKeySerialize pk2)) @? "Public keys must be equals"
    -- *** ---
    , testCase "Hashing" $ do
        (hexify $ hash256 "\1\2\3") @?= "039058c6f2c0cb492c533b0a4d14ef77cc0f78abccced5287d84a1a2011cfb81"
    -- *** ---
    , testCase "Aggregations" $ do
        let message = "My test message"
            msgHash = hash256 message
            sk1 = privateKeyFromSeed "rnd1"
            sk2 = privateKeyFromSeed "rnd2"
            sig1 = signInsecure sk1 message
            sig2 = signInsecure sk2 message
        insecureSignatureVerify sig1 [msgHash] [privateKeyGetPublicKey sk1] @? "Must be verifiable with sig1"
        insecureSignatureVerify sig2 [msgHash] [privateKeyGetPublicKey sk2] @? "Must be verifiable with sig2"
        let sigs = [sig1, sig2]
            pks =  [privateKeyGetPublicKey sk1, privateKeyGetPublicKey sk2]
            aggSig = insecureSignatureAggregate sigs
            aggPk  = publicKeyInsecureAggregate pks
        insecureSignatureVerify aggSig [msgHash] [aggPk] @? "Must be verifialbe with aggregated signatures"
    ]


hexify :: ByteString -> ByteString
hexify = BSL.toStrict . BS.toLazyByteString . BS.byteStringHex


dehexify :: ByteString -> ByteString
dehexify = BS.pack . go
  where
    go bs
        | BS.null bs = []
        | otherwise =
            let (b, e) = BS.splitAt 2 bs
                (b1:b2:_) = BS.unpack b
            in ((d b1) * 16 + (d b2)) : go e
    d :: Word8 -> Word8
    d w | fromIntegral (ord '0') <= w && w <= fromIntegral (ord '9') = w - fromIntegral (ord '0')
        | otherwise = (w - fromIntegral (ord 'a')) + 10

