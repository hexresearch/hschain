{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
-- |
module TM.Crypto (tests) where

import qualified Codec.Serialise as CBOR
import qualified Data.Aeson      as JSON
import Data.Typeable
import Data.Text      (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text       as T
import Test.Tasty
import Test.Tasty.HUnit
import System.Entropy (getEntropy)

import Thundermint.Crypto
import Thundermint.Crypto.Ed25519
import Thundermint.Crypto.SHA
import Thundermint.Crypto.Salsa20Poly1305


tests :: TestTree
tests = testGroup "Crypto"
  [ testsEd25519
  , testsSHA
  , testSalsa20
  ]

testsEd25519 :: TestTree
testsEd25519 = testGroup "ed25519"
  [ testCase "read . show = id @ Fingerprint/PublicKey/PrivKey Ed25519"
  $ do privK <- generatePrivKey @Ed25519
       let pubK = publicKey privK
           addr = fingerprint pubK
           blob = "ABCD"
           sign = signBlob privK blob
       privK @=? (read . show) privK
       pubK  @=? (read . show) pubK
       addr  @=? (read . show) addr
       sign  @=? (read . show) sign
    --
  , testCase "Signature OK (roundtrip)"
  $ do privK <- generatePrivKey @Ed25519
       let pubK = publicKey privK
           blob = "ABCD"
           sign = signBlob privK blob
       assertBool "Signature check failed" $ verifyBlobSignature pubK blob sign
    --
  , testCase "Signature OK (golden)"
  $ do let privK = read "\"HsBRtVUZ8ewZq5giWZHwr8crJm1iTVNLQeeyQk1vEmM8\""  :: PrivKey   Ed25519
           pubK  = read "\"9xeYWVLneMHJHEtewZe9X4nKbLAYjFEHR98q9CyhSxQN\""  :: PublicKey Ed25519
           sign  = read
             "Signature \
             \\"3R3ShiGCBPZNaPhyAtfaVX3V23YPHQABDxNn2F6qPwFANj1gUCBic6oBLGpSBXqpq1ZCKekA95ociGfVEdKhZhU7\""
           blob  = "ABCD"
       sign @=? signBlob privK blob
       assertBool "Signature check failed" $ verifyBlobSignature pubK blob sign
    --
  , testCase "Public key derivation is correct"
  $ do let k :: PrivKey Ed25519
           k = read "\"Cn6mra73QDNPkyf56Cfoxh9y9HDS8MREPw4GNcCxQb5Q\""
       read "\"5oxScYXwuzcCQRZ8QtUDzhxNdMx2g1nMabd4uPFPDdzi\"" @=? publicKey k
  , testCase "Fingerprint derivation is correct"
    --
  $ do let k :: PrivKey Ed25519
           k = read "\"Cn6mra73QDNPkyf56Cfoxh9y9HDS8MREPw4GNcCxQb5Q\""
       read "Fingerprint \"AhAM9SS8UQUbjrB3cwq9DMtb6mnyz61m9LuBr5kayq9q\"" @=? fingerprint (publicKey k)
    --
  , testCase "decodeBS . encodeBS = id"
  $ do privK <- generatePrivKey @Ed25519
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
  $ do privK <- generatePrivKey @Ed25519
       let pubK = publicKey privK
           addr = fingerprint pubK
           blob = "ABCD"
           sign = signBlob privK blob
       Just privK @=? (decodeBase58 . encodeBase58) privK
       Just pubK  @=? (decodeBase58 . encodeBase58) pubK
       Just addr  @=? (decodeBase58 . encodeBase58) addr
       Just sign  @=? (decodeBase58 . encodeBase58) sign
    --
  , testCase "CBOR roundtrip"
  $ do privK <- generatePrivKey @Ed25519
       let pubK = publicKey privK
           addr = fingerprint pubK
           blob = "ABCD"
           sign = signBlob privK blob
       Right privK @=? (CBOR.deserialiseOrFail . CBOR.serialise) privK
       Right pubK  @=? (CBOR.deserialiseOrFail . CBOR.serialise) pubK
       Right addr  @=? (CBOR.deserialiseOrFail . CBOR.serialise) addr
       Right sign  @=? (CBOR.deserialiseOrFail . CBOR.serialise) sign
    --
  , testCase "Aeson roundtrip"
  $ do privK <- generatePrivKey @Ed25519
       let pubK = publicKey privK
           addr = fingerprint pubK
           blob = "ABCD"
           sign = signBlob privK blob
       Just privK @=? (JSON.decode . JSON.encode) privK
       Just pubK  @=? (JSON.decode . JSON.encode) pubK
       Just addr  @=? (JSON.decode . JSON.encode) addr
       Just sign  @=? (JSON.decode . JSON.encode) sign
    --
  , testCase "encodeBase58 is Show compatible"
  $ do privK <- generatePrivKey @Ed25519
       let pubK = publicKey privK
       show privK @=? T.unpack ("\"" <> encodeBase58 privK <> "\"")
       show pubK  @=? T.unpack ("\"" <> encodeBase58 pubK  <> "\"")
    --
  , testCase "Sizes are correct"
  $ do privK <- generatePrivKey @Ed25519
       let pubK             = publicKey privK
           Fingerprint addr = fingerprint pubK
           blob             = "ABCD"
           Signature s      = signBlob privK blob
           ed               = Proxy :: Proxy Ed25519
       BS.length addr               @=? fingerprintSize ed
       BS.length (encodeToBS pubK ) @=? publicKeySize   ed
       BS.length (encodeToBS privK) @=? privKeySize     ed
       BS.length s                  @=? signatureSize   ed
  ]

testsSHA :: TestTree
testsSHA = testGroup "SHA"
  [ testHash (Proxy @SHA1)
    "D7AcZn7ouyHBGZesAmbH9aJjGsB"
  , testHash (Proxy @SHA256)
    "3vY84Vb3FUXnBxvjKQ6uGxrvRsMSm6nRHTT79bbGuzxh"
  , testHash (Proxy @SHA384)
    "212nrAU3Wf4tsoxRSTmEZGU24f67cAYG15rWMExaET1XEoy2Mcqm5zcQMBvwjDDWK9"
  , testHash (Proxy @SHA512)
    "wW3fpggshbTYibV8VHrz6ZFvs3EJig7geZTQvfy6FuVCsGDvamcKZHtV2TQMMjSU5i3TuzXSSjwsqGqR9aK1S5F"
  , testHash (Proxy @(SHA256 :<<< SHA512))
    "5V5rmaBpjNYQsC1Tv2pehRWzjX2G1LFPLR3K1RwsNevs"
  ]

testHash :: forall alg. (Typeable alg, CryptoHash alg) => Proxy alg -> Text -> TestTree
testHash p base58 = testGroup (show (typeRep p))
  [ testHashReadShow p
  , testHashSize     p
  , testCase "Hash algorithm is correct"
  $ do let h = hashBlob "ABCDF" :: Hash alg
       decodeBase58 base58 @=? Just h
  ]

testHashReadShow :: forall alg. (Typeable alg, CryptoHash alg) => Proxy alg -> TestTree
testHashReadShow p
  = testCase ("read . show = id @ Hash " ++ show (typeRep p))
  $ do let x = hashBlob "ABCD" :: Hash alg
       x @=? (read . show) x

testHashSize :: forall alg. (Typeable alg, CryptoHash alg) => Proxy alg -> TestTree
testHashSize p
  = testCase ("Size of Hash " ++ show (typeRep p) ++ " is correct")
  $ do let Hash bs = hashBlob "ABCD" :: Hash alg
       BS.length bs @=? hashSize p



testSalsa20 :: TestTree
testSalsa20 = testGroup "Salsa20Poly1305"
  [ testCase "Decoding works" $ do
      let key :: CypherKey Salsa20Poly1305
          Just key        = decodeFromBS =<< decodeB64 "lt8thW/fKVtoQ63/f3k9vLxBRxHTSbj2ZKa9bM3uXOU="
          Just nonce      = decodeFromBS =<< decodeB64 "z62h/TTmeH3s7pDX3NoiUcZjsLAUbd7F"
          cleartext       = "Lorem ipsum dolor amet"
          Just cyphertext = decodeB64 "wUr1JSEU2YogthaetNAiuPppjsw31HHTjYikwmFJ0domMi1Uleo="
      cyphertext     @=? encryptMessage key nonce cleartext
      Just cleartext @=? decryptMessage key nonce cyphertext
  --
  , testCase "Decoding roundtrip" $ do
      key   <- generateCypherKey   @Salsa20Poly1305
      nonce <- generateCypherNonce @Salsa20Poly1305
      cleartext <- getEntropy 123
      Just cleartext @=? decryptMessage key nonce (encryptMessage key nonce cleartext)
  --
  , testCase "decodeBS . encodeBS = id"
  $ do key   <- generateCypherKey   @Salsa20Poly1305
       nonce <- generateCypherNonce @Salsa20Poly1305
       Just key   @=? (decodeFromBS . encodeToBS) key
       Just nonce @=? (decodeFromBS . encodeToBS) nonce
  --
  , testCase "Sizes are correct"
  $ do key   <- generateCypherKey   @Salsa20Poly1305
       nonce <- generateCypherNonce @Salsa20Poly1305
       let ed = Proxy @Salsa20Poly1305
       BS.length (encodeToBS key  ) @=? cypherKeySize   ed
       BS.length (encodeToBS nonce) @=? cypherNonceSize ed
  ]

decodeB64 :: BS.ByteString -> Maybe BS.ByteString
decodeB64 bs = case B64.decode bs of
  Right x -> Just x
  Left  _ -> Nothing
