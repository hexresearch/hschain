{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module TM.BLS (tests) where


import Data.Typeable
import qualified Codec.Serialise as CBOR
import qualified Data.Aeson      as JSON
import qualified Data.ByteString as BS
import qualified Data.Text       as T

import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Crypto
import HSChain.Crypto.BLS
import qualified Crypto.Bls as RawBls


tests :: TestTree
tests = testGroup "BLS"
  [ testGroup "Asymmetric" $ testsAsymmetricCrypto (Proxy @BLS)
  , testGroup "Signatures" $ testsSignatureCrypto  (Proxy @BLS)
  , testGroup "BLS features"
        [ testCase "sizes" $ do
            (publicKeySize (undefined :: PublicKey BLS)) @?= RawBls.publicKeySizeGet
            (privKeySize   (undefined :: PrivKey   BLS)) @?= RawBls.privateKeySizeGet
            (signatureSize (undefined :: Signature BLS)) @?= RawBls.signatureSizeGet
        , testCase "signatures for hash 1" $ do
            (privKey :: PrivKey BLS) <- generatePrivKey
            let msg = "Please, don't hash me!"
                msgHash = hashBlobBLS msg
            let sig = signHash privKey msgHash
            (verifyHashSignature (publicKey privKey) msgHash sig) @? "Verify must be passed"
        , testCase "signatures for hash 2" $ do
            (privKey :: PrivKey BLS) <- generatePrivKey
            let msg = "I'm just a silly bytestring, why do you hash me?"
                msgHash = hashBlobBLS msg
            let sig = signHash privKey msgHash
            (verifyBlobSignature (publicKey privKey) msg sig) @? "Verify must be passed"
        , testCase "signatures for hash 3" $ do
            (privKey :: PrivKey BLS) <- generatePrivKey
            let msg = "So... You try to hash me again..."
                msgHash = hashBlobBLS msg
            let sig1 = signBlob privKey msg
            let sig2 = signHash privKey msgHash
            sig1 @?= sig2
        , testCase "signatures verify 1" $ do
            privKey :: PrivKey BLS <- generatePrivKey
            let sig = signBlob privKey "Check"
            verifyBlobSignature (publicKey privKey) "Check" sig @? "Verify must be passed"
        , testCase "signatures verify 2" $ do
            privKey :: PrivKey BLS <- generatePrivKey
            let sig = signBlob privKey "Can it pass the verification?"
            (not $ verifyBlobSignature (publicKey privKey) "No, sorry" sig) @? "Verify can't be passed"
        , testCase "signatures for hash 1" $ do
            privKey :: PrivKey BLS <- generatePrivKey
            let msg = "Please, don't hash me!"
                msgHash = hashBlobBLS msg
            let sig = signHash privKey msgHash
            (verifyHashSignature (publicKey privKey) msgHash sig) @? "Verify must be passed"
        , testCase "signatures for hash 2" $ do
            privKey :: PrivKey BLS <- generatePrivKey
            let msg = "I'm just a silly bytestring, why do you hash me?"
                msgHash = hashBlobBLS msg
            let sig = signHash privKey msgHash
            (verifyBlobSignature (publicKey privKey) msg sig) @? "Verify must be passed"
        , testCase "signatures for hash 3" $ do
            privKey :: PrivKey BLS <- generatePrivKey
            let msg = "So... You try to hash me again?"
                msgHash = hashBlobBLS msg
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
            (read "Hash \"cmzQwNFTPXVA9A9tG8TYuFo5vuVN9tFUoNC6DZBW3BW\"") @?= (hashBlobBLS "Am I too long to use in raw presentation?" :: Hash BLS)
        -- , testCase "aggregate" $ do
        --     let msg = "Abcdef"
        --     privKey1 :: PrivKey BLS <- generatePrivKey
        --     privKey2 :: PrivKey BLS <- generatePrivKey
        --     privKey3 :: PrivKey BLS <- generatePrivKey
        --     let pubKey1 = publicKey privKey1
        --         pubKey2 = publicKey privKey2
        --         pubKey3 = publicKey privKey3
        --         sig1 = signBlob privKey1 msg
        --         sig2 = signBlob privKey2 msg
        --         sig3 = signBlob privKey3 msg
        --         --
        --         aggPk = aggregatePublicKeys [pubKey1, pubKey2, pubKey3]
        --         aggSn = aggregateSignatures [sig1, sig2, sig3]
        --     (verifyBlobSignature aggPk msg aggSn) @? "Verify must be passed"
        -- , testCase "aggregate - order unimportant" $ do
        --     let msg = "Abcdef"
        --     (privKey1 :: PrivKey BLS) <- generatePrivKey
        --     (privKey2 :: PrivKey BLS) <- generatePrivKey
        --     (privKey3 :: PrivKey BLS) <- generatePrivKey
        --     let pubKey1 = publicKey privKey1
        --         pubKey2 = publicKey privKey2
        --         pubKey3 = publicKey privKey3
        --         sig1 = signBlob privKey1 msg
        --         sig2 = signBlob privKey2 msg
        --         sig3 = signBlob privKey3 msg
        --         --
        --         aggPk = aggregatePublicKeys [pubKey3, pubKey2, pubKey1]
        --         aggSn = aggregateSignatures [sig3, sig1, sig2]
        --     (verifyBlobSignature aggPk msg aggSn) @? "Verify must be passed"
        -- , testCase "aggregate - incorrect pk" $ do
        --     let msg = "Abcdef"
        --     privKey1 :: PrivKey BLS <- generatePrivKey
        --     privKey2 :: PrivKey BLS <- generatePrivKey
        --     privKey3 :: PrivKey BLS <- generatePrivKey
        --     privKey4 :: PrivKey BLS <- generatePrivKey
        --     let pubKey1  = publicKey privKey1
        --         pubKey2  = publicKey privKey2
        --         _pubKey3 = publicKey privKey3
        --         incorrectPubKey4 = publicKey privKey4
        --         sig1 = signBlob privKey1 msg
        --         sig2 = signBlob privKey2 msg
        --         sig3 = signBlob privKey3 msg
        --         --
        --         aggPk = aggregatePublicKeys [pubKey1, pubKey2, incorrectPubKey4]
        --         aggSn = aggregateSignatures [sig1, sig2, sig3]
        --     (not $ verifyBlobSignature aggPk msg aggSn) @? "Verify must not be passed"
        -- , testCase "aggregate - incorrect sk" $ do
        --     let msg = "Abcdef"
        --         incorrectMsg = "ZYX"
        --     privKey1 :: PrivKey BLS <- generatePrivKey
        --     privKey2 :: PrivKey BLS <- generatePrivKey
        --     privKey3 :: PrivKey BLS <- generatePrivKey
        --     let pubKey1 = publicKey privKey1
        --         pubKey2 = publicKey privKey2
        --         pubKey3 = publicKey privKey3
        --         sig1  = signBlob privKey1 msg
        --         sig2  = signBlob privKey2 msg
        --         _sig3 = signBlob privKey3 msg
        --         incorrectSig4 = signBlob privKey3 incorrectMsg
        --         --
        --         aggPk = aggregatePublicKeys [pubKey1, pubKey2, pubKey3]
        --         aggSn = aggregateSignatures [sig1, sig2, incorrectSig4]
        --     (not $ verifyBlobSignature aggPk msg aggSn) @? "Verify must not be passed"
        --
        -- TODO перенести сюда остальные тесты с точными значениями
        ]
  ]


----------------------------------------------------------------
-- Generic tests copied from hschain-crypto
----------------------------------------------------------------


testsAsymmetricCrypto
  :: forall alg. ( CryptoAsymmetric alg, Eq (PrivKey alg)
                 , ByteReprSized (PublicKey alg), ByteReprSized (PrivKey alg))
  => Proxy alg -> [TestTree]
testsAsymmetricCrypto tag =
  [ testGroup "PrivKey"   $ testsStdIntances (Proxy @(PrivKey   alg))
  , testGroup "PublicKey" $ testsStdIntances (Proxy @(PublicKey alg))
    --
  , testCase "encodeBase58 is Show compatible"
  $ do privK <- generatePrivKey @alg
       let pubK = publicKey privK
       show privK @=? T.unpack ("\"" <> encodeBase58 privK <> "\"")
       show pubK  @=? T.unpack ("\"" <> encodeBase58 pubK  <> "\"")
    --
  , testCase "Sizes are correct (Private key)"
  $ do privK <- generatePrivKey @alg
       privKeySize   tag @=? BS.length (encodeToBS privK)
    --
  , testCase "Sizes are correct (Public key)"
  $ do privK <- generatePrivKey @alg
       let pubK = publicKey privK
       publicKeySize tag @=? BS.length (encodeToBS pubK )
  ]


testsSignatureCrypto
  :: forall alg. (CryptoSign alg, ByteReprSized (Signature alg))
  => Proxy alg -> [TestTree]
testsSignatureCrypto tag =
  [ testGroup "Signature"   $ testsStdIntances (Proxy @(Signature   alg))
    --
  , testCase "Signature OK (roundtrip)"
  $ do privK <- generatePrivKey @alg
       let pubK = publicKey privK
           blob = "ABCD"
           sign = signBlob privK blob
       assertBool "Signature check failed" $ verifyBlobSignature pubK blob sign
    --
  , testCase "Sizes are correct"
  $ do sign <- generateIO @(Signature   alg)
       BS.length (encodeToBS sign) @=? signatureSize   tag
  ]


testsStdIntances
  :: forall a. (Generate a
               , Show a, Read a, Eq a
               , JSON.ToJSON a, JSON.FromJSON a, CBOR.Serialise a
               , ByteRepr a
               )
  => Proxy a -> [TestTree]
testsStdIntances _ =
  [ testCase "read . show = id"
  $ do a <- generateIO @a
       a @=? (read . show) a
  --
  , testCase "decodeBS . encodeBS = id"
  $ do a <- generateIO @a
       Just a @=? (decodeFromBS . encodeToBS) a
  --
  , testCase "decodeBase58 . encodeBase58 = id"
  $ do a <- generateIO @a
       Just a @=? (decodeBase58 . encodeBase58) a
    --
  , testCase "CBOR roundtrip"
  $ do a <- generateIO @a
       Right a @=? (CBOR.deserialiseOrFail . CBOR.serialise) a
    --
  , testCase "Aeson roundtrip"
  $ do a <- generateIO @a
       Just a @=? (JSON.decode . JSON.encode) a
  ]


-- Helper type class for generation test sameples
class Generate a where
  generateIO :: IO a

instance CryptoAsymmetric alg => Generate (PrivKey alg) where
  generateIO = generatePrivKey
instance CryptoAsymmetric alg => Generate (PublicKey alg) where
  generateIO = publicKey <$> generatePrivKey
instance CryptoSign alg => Generate (Signature alg) where
  generateIO = do pk <- generatePrivKey
                  return $! signBlob pk "ABCD"
-- instance CryptoDH alg => Generate (DHSecret alg) where
--  generateIO = diffieHelman <$> generateIO <*> generateIO

