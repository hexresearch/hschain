{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
module TM.Crypto (tests) where

import qualified Codec.Serialise as CBOR
import qualified Data.Aeson      as JSON
import Data.Typeable
import Data.Text      (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text       as T
import Test.Tasty
import Test.Tasty.HUnit

import HSChain.Crypto
import HSChain.Crypto.Ed25519
import HSChain.Crypto.Curve25519
import HSChain.Crypto.SHA
import HSChain.Crypto.Salsa20Poly1305
import HSChain.Crypto.KDFNaCl


tests :: TestTree
tests = testGroup "Crypto"
  [ testsEd25519
  , testsCurve25519
  , testsSHA
  , testSalsa20
  , testsNaClBox
  , testsHMAC
  ]

testsEd25519 :: TestTree
testsEd25519 = testGroup "ed25519"
  [ testGroup "Asymmetric" $ testsAsymmetricCrypto (Proxy @Ed25519)
  , testGroup "Signatures" $ testsSignatureCrypto  (Proxy @Ed25519)
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
  $ do let k :: PrivKey Ed25519
           k = read "\"Cn6mra73QDNPkyf56Cfoxh9y9HDS8MREPw4GNcCxQb5Q\""
           fp :: Fingerprint (SHA256 :<<< SHA512) Ed25519
           fp = fingerprint (publicKey k)
       read "Fingerprint \"AhAM9SS8UQUbjrB3cwq9DMtb6mnyz61m9LuBr5kayq9q\"" @=? fp
  ]

testsCurve25519 :: TestTree
testsCurve25519 = testGroup "Curve25519"
  [ testGroup "Asymmetric"     $ testsAsymmetricCrypto (Proxy @Curve25519)
  , testGroup "Diffie-Hellman" $ testsDHCrypto         (Proxy @Curve25519)
  , testCase  "DH secrec calculated corectly"
  $ do let k1,k2 :: PrivKey Curve25519
           Just k1 = decodeFromBS $ BS.pack
             [7, 72, 169, 73, 9, 245, 29, 176, 157, 135, 28, 208, 153, 120, 191, 175
             , 157, 231, 38, 52, 198, 137, 196, 64, 131, 234, 207, 71, 114, 78, 159, 168]
           Just k2 = decodeFromBS $ BS.pack
             [91, 18, 95, 167, 15, 132, 156, 104, 150, 91, 173, 110, 70, 36, 98, 57
             , 7, 12, 167, 135, 238, 128, 112, 220, 56, 184, 48, 34, 0, 96, 65, 61]
           Just dh = decodeFromBS $ BS.pack
             [ 165, 79, 158, 94, 144, 197, 201, 61, 185, 4, 199, 97, 73, 130, 159, 233
             , 23, 6, 122, 248, 184, 217, 216, 36, 106, 12, 205, 198, 240, 200, 48, 63
             ]
       dh @=? diffieHelman (publicKey k1) k2
       dh @=? diffieHelman (publicKey k2) k1
  ]


testsNaClBox :: TestTree
testsNaClBox = testGroup "Tests for NaCl box"
  [ testCase "We correcttly work with box"
  $ do let k1,k2 :: PrivKey Curve25519
           Just k1 = decodeFromBS =<< decodeB64 "to7g9QMuKT68la5EeU69v5FoDSGkp3cszDiLdmcxMeU="
           Just k2 = decodeFromBS =<< decodeB64 "0TVBDcAS1AwDyNpjUpwPgoiPm1uOcBmTNVxPiz6Q3gk="
           pubK1   = publicKey k1
           pubK2   = publicKey k2
           -- Encrypted data
           cleartext       = "abcd"
           Just nonce      = decodeFromBS =<< decodeB64 "W0o/q7paOp2CBj0s0+KQfVyZ4q9Ugu33"
           Just cyphertext = decodeB64 "ogaTrD3fEcn/4U+IHukyWkxs/yw="
           --
           box :: PubKeyBox Curve25519 KDFNaCl Salsa20Poly1305
           box = PubKeyBox cyphertext nonce
       Just cleartext @=? openPubKeyBox k1 pubK2 box
       Just cleartext @=? openPubKeyBox k2 pubK1 box
  ]

testsAsymmetricCrypto
  :: forall alg. (CryptoAsymmetric alg, Eq (PrivKey alg))
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
  :: forall alg. CryptoSign alg
  => Proxy alg -> [TestTree]
testsSignatureCrypto tag =
  [ testGroup "Fingerprint" $ testsStdIntances (Proxy @(Fingerprint SHA512 alg))
  , testGroup "Signature"   $ testsStdIntances (Proxy @(Signature   alg))
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


testsDHCrypto
  :: forall alg. (CryptoDH alg, Eq (DHSecret alg))
  => Proxy alg -> [TestTree]
testsDHCrypto _ =
  [ testGroup "DHSecret" $ testsStdIntances (Proxy @(DHSecret alg))
  --
  , testCase "Diffie–Hellman key exchange works"
  $ do k1 <- generatePrivKey @alg
       k2 <- generatePrivKey @alg
       diffieHelman (publicKey k1) k2 @=? diffieHelman (publicKey k2) k1
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
instance (CryptoHash hash, CryptoAsymmetric alg) => Generate (Fingerprint hash alg) where
  generateIO = fingerprint . publicKey <$> generatePrivKey
instance CryptoSign alg => Generate (Signature alg) where
  generateIO = do pk <- generatePrivKey
                  return $! signBlob pk "ABCD"
instance CryptoDH alg => Generate (DHSecret alg) where
  generateIO = diffieHelman <$> generateIO <*> generateIO


----------------------------------------------------------------
-- Generic tests for hashes
----------------------------------------------------------------

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


----------------------------------------------------------------
-- Tests for cyphers
----------------------------------------------------------------

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
      let  cleartext = "ASDF"
      Just cleartext @=? decryptMessage key nonce (encryptMessage key nonce cleartext)
  --
  , testCase "Tampering detected" $ do
      key   <- generateCypherKey   @Salsa20Poly1305
      nonce <- generateCypherNonce @Salsa20Poly1305
      let cleartext   = "ASDF"
          cyphertext  = encryptMessage key nonce cleartext
          cyphertext' = BS.take (BS.length cyphertext - 1) cyphertext
      Nothing @=? decryptMessage key nonce cyphertext'
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

----------------------------------------------------------------
-- Tests for HMAC
----------------------------------------------------------------

testsHMAC :: TestTree
testsHMAC = testGroup "HMAC"
  [ testHMAC @SHA1
    "e2a7671c074e39a5df933a44cbce8f9d88145f7d"
  , testHMAC @SHA256
    "8552e36de7567f917d99ce866a0b9837d1f2a892e4fc75eb74133c2d453a802f"
  , testHMAC @SHA384
    "2d51b05e3cb71a55a010a6c7799a34e2c5422e11851497b591ed239ce92a1d6a\
    \d36cc0990066c5b3d1aef16b73f69946"
  , testHMAC @SHA512
    "67d12aac1ce3b9fa4a707384c07731f30c68810b1d971b2550c2a8708e59ba5d\
    \cd86c2664bde4eb3e0f653c40619bdff81fd18efeecdfea769f960ffba600e38"
  ]

testHMAC :: forall alg. (Typeable alg, CryptoHMAC alg)
         => BS.ByteString -> TestTree
testHMAC str = testGroup (show (typeRep (Proxy @alg)))
  [ testCase "HMAC is correct"
  $ let Just expected = decodeFromBS $ fst $ B16.decode str
    in expected @=? mac
  --
  , testCase "Size is correct"
  $ hashSize (Proxy @alg) @=? BS.length (encodeToBS mac)
  ]
  where
    mac :: HMAC alg
    mac = hmac "KEY" "asdf"
