{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Criterion.Main
import Prelude

--import Control.DeepSeq
--import Control.Exception (evaluate)

--import Crypto.Random              (getRandomBytes)
--import Thundermint.Crypto
--import Thundermint.Crypto.Ed25519


import ValidatorSetsBench

--kB :: Int
--kB = 1024

main :: IO ()
main = defaultMain [ benchValidatorSets ]
{-
  !prk <- generatePrivKey
  let !pk = publicKey prk
  !blobs <- force . replicate 10000 <$> getRandomBytes kB
  !signatures <- evaluate $ force $ map (signBlob prk) blobs
  !blobWithSign <- evaluate $ force $ zip blobs signatures
  defaultMain [
    bgroup "1 block"
                 [ bench "signing" $ whnf (signBlob prk) (head blobs)
                 , bench "verifying"  $ whnf (uncurry (verifyBlobSignature pk)) (head blobWithSign)
                 ],
    bgroup "10 blocks"
                 [ bench "signing" $ nf (map (signBlob prk)) $ take 10 blobs
                 , bench "verifying"  $ nf (map (uncurry (verifyBlobSignature pk))) $ take 10 blobWithSign
                 ],
    bgroup "100 blocks"
                 [ bench "signing" $ nf (map (signBlob prk)) $ take 100 blobs
                 , bench "verifying"  $ nf (map (uncurry (verifyBlobSignature pk))) $ take 100 blobWithSign
                 ],
    bgroup "10000 blocks"
                 [ bench "signing" $ nf (map (signBlob prk)) blobs
                 , bench "verifying"  $ nf (map (uncurry (verifyBlobSignature pk))) blobWithSign
                 ]
    ]
-}
