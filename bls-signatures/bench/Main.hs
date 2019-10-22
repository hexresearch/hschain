{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main


import Crypto.Bls

import Data.ByteString as BS
import qualified Data.Vector as V


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


hash :: BS.ByteString -> IO Hash256
hash = hash256


benchSignInsecurePrehashed :: PrivateKey -> Hash256 -> IO InsecureSignature
benchSignInsecurePrehashed = signInsecurePrehashed

benchVerifyInsecure :: PublicKey -> Hash256 -> InsecureSignature -> IO Bool
benchVerifyInsecure pubKey msgHash sig =
    verifyInsecure sig (V.singleton msgHash) (V.singleton pubKey)

benchVerifyInsecure1 :: PublicKey -> Hash256 -> InsecureSignature -> IO Bool
benchVerifyInsecure1 pubKey msgHash sig =
    verifyInsecure1 sig msgHash pubKey

benchSig :: BS.ByteString -> IO Bool
benchSig message = do
    sk1 <- fromSeed "abc"
    sk2 <- fromSeed "def"
    pk1 <- getPublicKey sk1
    pk2 <- getPublicKey sk2
    messageHash <- hash256 message
    sig1 <- signInsecure sk1 message
    sig2 <- signInsecure sk2 message
    b1 <- verifyInsecure sig1 (V.singleton messageHash) (V.singleton pk1)
    b2 <- verifyInsecure sig2 (V.singleton messageHash) (V.singleton pk2)
    aggSig <- aggregateInsecureSignatures (V.fromList [sig1, sig2])
    aggPk  <- aggregateInsecurePublicKey  (V.fromList [pk1,  pk2])
    b3 <- verifyInsecure aggSig (V.singleton messageHash) (V.singleton aggPk)
    --
    return $! b1 && b2 && b3


-- Our benchmark harness.
main :: IO ()
main = do
    privKey <- fromSeed "abc"
    pubKey  <- getPublicKey privKey
    msgHash <- hash256 bigMessage2
    sig     <- signInsecurePrehashed privKey msgHash
    defaultMain
        [
          bgroup "hash" [ bench "small 1"  $ nfIO (hash smallMessage1)
                        , bench "small 2"  $ nfIO (hash smallMessage2)
                        , bench "small 3"  $ nfIO (hash smallMessage3)
                        , bench "medium 1" $ nfIO (hash mediumMessage1)
                        , bench "medium 2" $ nfIO (hash mediumMessage2)
                        , bench "medium 3" $ nfIO (hash mediumMessage3)
                        , bench "big 1"    $ nfIO (hash bigMessage1)
                        , bench "big 2"    $ nfIO (hash bigMessage2)
                        , bench "big 3"    $ nfIO (hash bigMessage3)
                        ]
        , bgroup "sig"  [ bench "1"  $ nfIO (benchSig smallMessage1)
                        , bench "2"  $ nfIO (benchSig mediumMessage1)
                        , bench "3"  $ nfIO (benchSig bigMessage1)
                        ]
        , bgroup "sig"  [ bench "sign"   $ nfIO (benchSignInsecurePrehashed privKey msgHash)
                        , bench "verify" $ nfIO (benchVerifyInsecure pubKey msgHash sig)
                        , bench "verify1" $ nfIO (benchVerifyInsecure1 pubKey msgHash sig)
                        ]
        ]
