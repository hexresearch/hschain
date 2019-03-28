{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
-- |
module TM.Crypto (tests) where

import Data.Typeable
import qualified Data.ByteString as BS
import qualified Data.Text       as T
import Test.Tasty
import Test.Tasty.HUnit

import Thundermint.Crypto
import Thundermint.Crypto.Ed25519

tests :: TestTree
tests = testGroup "Crypto"
  [ testCase "read . show = id @ Fingerprint/PublicKey/PrivKey Ed25519_SHA512"
  $ do privK <- generatePrivKey @Ed25519_SHA512
       let pubK = publicKey privK
           addr = fingerprint pubK
           blob = "ABCD"
           sign = signBlob privK blob
       privK @=? (read . show) privK
       pubK  @=? (read . show) pubK
       addr  @=? (read . show) addr
       sign  @=? (read . show) sign
    --
  , testCase "read . show = id @ Hash Ed25519_SHA512"
  $ do let x = hashBlob "ABCD" :: Hash Ed25519_SHA512
       x @=? (read . show) x
    --
  , testCase "Signature OK (roundtrip)"
  $ do privK <- generatePrivKey @Ed25519_SHA512
       let pubK = publicKey privK
           blob = "ABCD"
           sign = signBlob privK blob
       assertBool "Signature check failed" $ verifyBlobSignature pubK blob sign
    --
  , testCase "Signature OK (golden)"
  $ do let privK = read "\"HsBRtVUZ8ewZq5giWZHwr8crJm1iTVNLQeeyQk1vEmM8\""  :: PrivKey   Ed25519_SHA512
           pubK  = read "\"9xeYWVLneMHJHEtewZe9X4nKbLAYjFEHR98q9CyhSxQN\""  :: PublicKey Ed25519_SHA512
           sign  = read
             "Signature \
             \\"3R3ShiGCBPZNaPhyAtfaVX3V23YPHQABDxNn2F6qPwFANj1gUCBic6oBLGpSBXqpq1ZCKekA95ociGfVEdKhZhU7\""
           blob  = "ABCD"
       sign @=? signBlob privK blob
       assertBool "Signature check failed" $ verifyBlobSignature pubK blob sign
    --
  , testCase "Hash algorithm is correct"
  $ do let h = hashBlob "ABCDF" :: Hash Ed25519_SHA512
       read ("Hash \"wW3fpggshbTYibV8VHrz6ZFvs3EJig7geZTQvfy6FuVCsGDvamcKZHtV2TQMMjSU5i3TuzXSSjwsqGqR9aK1S5F\"")
         @=? h
    --
  , testCase "Public key derivation is correct"
  $ do let k :: PrivKey Ed25519_SHA512
           k = read "\"Cn6mra73QDNPkyf56Cfoxh9y9HDS8MREPw4GNcCxQb5Q\""
       read "\"5oxScYXwuzcCQRZ8QtUDzhxNdMx2g1nMabd4uPFPDdzi\"" @=? publicKey k
  , testCase "Fingerprint derivation is correct"
    --
  $ do let k :: PrivKey Ed25519_SHA512
           k = read "\"Cn6mra73QDNPkyf56Cfoxh9y9HDS8MREPw4GNcCxQb5Q\""
       read "Fingerprint \"AhAM9SS8UQUbjrB3cwq9DMtb6mnyz61m9LuBr5kayq9q\"" @=? fingerprint (publicKey k)
    --
  , testCase "decodeBS . encodeBS = id"
  $ do privK <- generatePrivKey @Ed25519_SHA512
       let pubK = publicKey privK
           addr = fingerprint pubK
           blob = "ABCD"
           sign = signBlob privK blob
       Just privK @=? (decodeFromBS . encodeToBS) privK
       Just pubK  @=? (decodeFromBS . encodeToBS) pubK
       Just addr  @=? (decodeFromBS . encodeToBS) addr
       Just sign  @=? (decodeFromBS . encodeToBS) sign
    --
  , testCase "decodeBase58 . encodeBase58 = id"
  $ do privK <- generatePrivKey @Ed25519_SHA512
       let pubK = publicKey privK
           addr = fingerprint pubK
           blob = "ABCD"
           sign = signBlob privK blob
       Just privK @=? (decodeBase58 . encodeBase58) privK
       Just pubK  @=? (decodeBase58 . encodeBase58) pubK
       Just addr  @=? (decodeBase58 . encodeBase58) addr
       Just sign  @=? (decodeBase58 . encodeBase58) sign
    --
  , testCase "encodeBase58 is Show compatible"
  $ do privK <- generatePrivKey @Ed25519_SHA512
       let pubK = publicKey privK
       show privK @=? T.unpack ("\"" <> encodeBase58 privK <> "\"")
       show pubK  @=? T.unpack ("\"" <> encodeBase58 pubK  <> "\"")
    --
  , testCase "Sizes are correct"
  $ do privK <- generatePrivKey @Ed25519_SHA512
       let pubK             = publicKey privK
           Fingerprint addr = fingerprint pubK
           blob             = "ABCD"
           Signature s      = signBlob privK blob
           Hash bs          = hash () :: Hash Ed25519_SHA512
           ed               = Proxy :: Proxy Ed25519_SHA512
       BS.length bs                  @=? hashSize        ed
       BS.length addr                @=? fingerprintSize ed
       BS.length (encodeToBS  pubK ) @=? publicKeySize   ed
       BS.length (encodeToBS privK)  @=? privKeySize     ed
       BS.length s                   @=? signatureSize   ed
  ]
