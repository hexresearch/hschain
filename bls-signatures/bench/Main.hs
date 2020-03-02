{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Criterion.Main


import Crypto.Bls

import Data.ByteString as BS


smallMessage1 :: BS.ByteString
smallMessage1 = mkMsg 8

smallMessage2 :: BS.ByteString
smallMessage2 = mkMsg 16

smallMessage3 :: BS.ByteString
smallMessage3 = mkMsg 32

mediumMessage1 :: BS.ByteString
mediumMessage1 = mkMsg 128

mediumMessage2 :: BS.ByteString
mediumMessage2 = mkMsg 256

mediumMessage3 :: BS.ByteString
mediumMessage3 = mkMsg 512

bigMessage1 :: BS.ByteString
bigMessage1 = mkMsg 8192

bigMessage2 :: BS.ByteString
bigMessage2 = mkMsg 131072

bigMessage3 :: BS.ByteString
bigMessage3 = mkMsg 1048576

mkMsg :: Int -> BS.ByteString
mkMsg sz = fst $ BS.unfoldrN sz (\i -> Just (i, i + 3)) 1


hash :: BS.ByteString -> Hash256
hash = hash256


benchSignInsecurePrehashed :: PrivateKey -> Hash256 -> InsecureSignature
benchSignInsecurePrehashed = signInsecurePrehashed


benchVerifyInsecure :: PublicKey -> Hash256 -> InsecureSignature -> Bool
benchVerifyInsecure pubKey msgHash sig =
    insecureSignatureVerify sig [msgHash] [pubKey]


benchSig :: BS.ByteString -> Bool
benchSig message =
    let sk1 = privateKeyFromSeed "abc"
        sk2 = privateKeyFromSeed "def"
        pk1 = privateKeyGetPublicKey sk1
        pk2 = privateKeyGetPublicKey sk2
        messageHash = hash256 message
        sig1 = signInsecure sk1 message
        sig2 = signInsecure sk2 message
        b1 = insecureSignatureVerify sig1 [messageHash] [pk1]
        b2 = insecureSignatureVerify sig2 [messageHash] [pk2]
        aggSig = insecureSignatureAggregate [sig1, sig2]
        aggPk  = publicKeyInsecureAggregate [pk1,  pk2]
        b3 = insecureSignatureVerify aggSig [messageHash] [aggPk]
    in (b1 && b2 && b3)


-- Our benchmark harness.
main :: IO ()
main = do
    let !privKey = privateKeyFromSeed "abc"
        !pubKey = privateKeyGetPublicKey privKey
        !msgHash = hash256 bigMessage2
        !sig     = signInsecurePrehashed privKey msgHash
    defaultMain
        [
          bgroup "hash" [ bench "small 1"  $ nf hash smallMessage1
                        , bench "small 2"  $ nf hash smallMessage2
                        , bench "small 3"  $ nf hash smallMessage3
                        , bench "medium 1" $ nf hash mediumMessage1
                        , bench "medium 2" $ nf hash mediumMessage2
                        , bench "medium 3" $ nf hash mediumMessage3
                        , bench "big 1"    $ nf hash bigMessage1
                        , bench "big 2"    $ nf hash bigMessage2
                        , bench "big 3"    $ nf hash bigMessage3
                        ]
        , bgroup "sig"  [ bench "1"  $ nf benchSig smallMessage1
                        , bench "2"  $ nf benchSig mediumMessage1
                        , bench "3"  $ nf benchSig bigMessage1
                        ]
        , bgroup "sig"  [ bench "sign"   $ nf (uncurry benchSignInsecurePrehashed) (privKey, msgHash)
                        , bench "verify" $ nf (\(p, m, s) -> benchVerifyInsecure p m s) (pubKey, msgHash, sig)
                        ]
        ]
