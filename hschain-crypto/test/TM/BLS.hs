{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TM.BLS (tests) where


import qualified Crypto.Bls as RawBls
import HSChain.Crypto
import HSChain.Crypto.BLS

import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "BLS"
    [ testCase "sizes" $ do
        (publicKeySize (undefined :: PublicKey BLS)) @?= RawBls.publicKeySize
        (privKeySize   (undefined :: PrivKey   BLS)) @?= RawBls.privateKeySize
        (signatureSize (undefined :: Signature BLS)) @?= RawBls.signatureSize
        (hashSize      (undefined :: Hash BLS))      @?= RawBls.hashSize
    , testCase "signatures verify 1" $ do
        (privKey :: PrivKey BLS) <- generatePrivKey
        let sig = signBlob privKey "Проверка"
        (verifyBlobSignature (publicKey privKey) "Проверка" sig) @? "Verify must be passed"
    , testCase "signatures verify 2" $ do
        (privKey :: PrivKey BLS) <- generatePrivKey
        let sig = signBlob privKey "Can it pass the verification?"
        (not $ verifyBlobSignature (publicKey privKey) "No, sorry" sig) @? "Verify can't be passed"
    , testCase "signatures for hash 1" $ do
        (privKey :: PrivKey BLS) <- generatePrivKey
        let msg = "Please, don't hash me!"
            msgHash = hashBlob msg
        let sig = signHash privKey msgHash
        (verifyHashSignature (publicKey privKey) msgHash sig) @? "Verify must be passed"
    , testCase "signatures for hash 2" $ do
        (privKey :: PrivKey BLS) <- generatePrivKey
        let msg = "I'm just a silly bytestring, why do you hash me?"
            msgHash = hashBlob msg
        let sig = signHash privKey msgHash
        (verifyBlobSignature (publicKey privKey) msg sig) @? "Verify must be passed"
    , testCase "signatures for hash 3" $ do
        (privKey :: PrivKey BLS) <- generatePrivKey
        let msg = "So... You try to hash me again?"
            msgHash = hashBlob msg
        let sig1 = signBlob privKey msg
        let sig2 = signHash privKey msgHash
        sig1 @?= sig2
    , testCase "read/show 1" $ do
        let (privKey :: PrivKey BLS) = read "\"3fDbEdCy2bauoADeCGZwB8ANT4r58274dpE2TKoGq3JV\""
        publicKey privKey @=? read "\"AwNfwYfCb5Vp2WBTbs7xjd6SVJmkv2RwP18xFDpkDHH3eCxZ5jHUWWWDUYqG7j2wp\""
    , testCase "read/show 2" $ do
        let (privKey :: PrivKey BLS) = read "\"6nH7Gjvo8t7qo4rpbzXVsTqUHNoawS9wDPqmdcjbgSAs\""
        let sig = signBlob privKey "Can it pass the verification?"
        sig @=? read "Signature \"2Tt5cbre1D8cuHmtWfUBiofUPtZrU9EvXc4RMGuyx4aEd3r7jNEqDSAgTeHpfKSBfVhbPmRxynhHzguVA6VVSv2bTJ2tyfGTVfPCFtvv3EhCmjYEeRzLLDGg2fbF9uaJTGB\""
    , testCase "read/show 3" $ do
        let msg = "Verify Me If You Can"
            wrongMsg = "Verify You If You Can"
            (pubKey :: PublicKey BLS) = read "\"JmPESv1nVz4T39ubqiCTyi27Zp2hrYXAPLSRpvWuTeRRR1yFKPX1tD5wTv7z4CTKY\""
            sig = read "Signature \"9op1EUKU98LPdyBheGucDDyeA4A9QKk9jG2XR5qyKfFu1hSapV2LyKi8YuoQt2Rtnt1nodwnaEYiXWWkUUMWijcabTy2YLAjKZcS5LHsdUaHF5Ex9VM2Ca3JwNdvhPFJSTN\""
        (verifyBlobSignature pubKey msg sig) @? "Verify must be passed"
        (not $ verifyBlobSignature pubKey wrongMsg sig) @? "Verify must not be passed"
    , testCase "read/show 4" $ do
        (read "Hash \"cmzQwNFTPXVA9A9tG8TYuFo5vuVN9tFUoNC6DZBW3BW\"") @?= (hashBlob "Am I too long to use in raw presentation?" :: Hash BLS)
    ]
