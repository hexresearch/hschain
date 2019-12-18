{-# LANGUAGE OverloadedStrings #-}
-- |
module Ben.BLS (benchmarks) where

import Criterion.Main
import qualified Data.ByteString as BS

import HSChain.Crypto
import HSChain.Crypto.BLS

----------------------------------------------------------------
--
----------------------------------------------------------------

benchmarks :: Benchmark
benchmarks = bgroup "Crypto"
  [ bench "Signing  (BLS)"                 $ nf (signBlob privKeyBLS) blob
  , bench "Checking (BLS)"                 $ nf (verifyBlobSignature pubKeyBLS blob) signBLS
  , bench "Signing hashed  (BLS)"          $ nf (signHash privKeyBLS) blobHash
  , bench "Checking hashed (BLS)"          $ nf (verifyHashSignature pubKeyBLS blobHash) signBLS
  , bench "Private keys aggregation (BLS)" $ nf aggregatePrivKeys   privKeysBLS
  , bench "Public keys aggregation  (BLS)" $ nf aggregatePublicKeys publicKeysBLS
  , bench "Signatures aggregation   (BLS)" $ nf aggregateSignatures signaturesBLS
  ]


blob :: BS.ByteString
blob = BS.replicate 40 33

blobHash :: Hash BLS
blobHash = hashBlobBLS blob

signBLS :: Signature BLS
signBLS = signBlob privKeyBLS blob

privKeyBLS :: PrivKey BLS
privKeyBLS = read "\"3fDbEdCy2bauoADeCGZwB8ANT4r58274dpE2TKoGq3JV\""

pubKeyBLS :: PublicKey BLS
pubKeyBLS = publicKey privKeyBLS


privKeysBLS :: [PrivKey BLS]
privKeysBLS = map read
    [ "\"7jKiMD9UNvbTSXVY7mm4WPnjx6wHNy7fxvmb2YWKX1rJ\""
    , "\"6RodYqEFkpUda9KbPEZAwvLnz7BEci5SndixZjnXRQF\""
    , "\"8CLVMBkbYv2WdfqQnHbgVUe9adyrs325aazFVAMx9nJA\""
    , "\"2ofYUYZUT86JBkH1FVEwADXw6HMNS3EG6vxLvPRAdJpy\""
    , "\"3czfgE7A12hcZSFum8cCre7g3WKr9LzMDPkkhBGUsDdd\""
    , "\"46ePTctzuDG1u8xGEMgQF6Q4KcpGxeZzuLcYf6HEwPNg\""
    , "\"4UxB43wb5JbZJrpsphdsB9XoD5gQ4xdJLxuCekaB2h2h\""
    , "\"6p1R2MLmj1gKR7roeqgabW2un1w9X7HQDx5T98PmUSfM\""
    , "\"4x7AkpLuqcpi7TCS4MufJ6LyLEYAhotjSCncpiZQ4R3v\""
    , "\"2W2gQKpWyBn5DkrE5ibkhsbYjJFjAafEpqJEUk4UQp2a\""
    , "\"6gYRiH4oWKJBZaQoJJyFvBesZy3ydVVUnBwSjVCmY6vQ\""
    , "\"7RND4avtcpRUU3RuyQovVzVPsrCn4Syx6WkYgFsdHtpe\""
    , "\"wFa47brSpoRxCcSya4xs2yXnxGBGMrwkxG3VfGaxqUX\""
    , "\"5E54uVMKUWhcjEsKSS2WaWu6jWuyEti4XFAzygQhRhky\""
    , "\"3fXCjYz5erwJn865JZUa78Kg3tExQBFEbChA83p3LAyh\""
    , "\"3zrxfPqEAz5GWb1H7JY1Gavm4nXgrwhKXxpZtEeLmxYE\""
    , "\"3KVj68vUEQbFKFEhMEki9hWSh2ejPW4wCSidMQZM2GAP\""
    , "\"7kDacxC4Q4beSx1bWX827CtsHVw46kxD4bxzCDdHXP5v\""
    , "\"77XThfENHSKxojahmGPXjiwqS9gU8AJzVzZnvbfwbiCu\""
    , "\"2nipbjUQjyqJNVnMFuN1RuEXwbNoL8DXFYjrQQ7mk3Yd\""
    ]


publicKeysBLS :: [PublicKey BLS]
publicKeysBLS = map read
    [ "\"6Xv8yaSMoyiYNJwRUu2PxchT5oogzPYGVPjo7cssqNjEY3sYEKpKs8jcuWrZ1Nu3De\""
    , "\"6UU1nmz3eJ5TVxQf1LfHT1se97PuNNVqbZZCNnW19x87FctbtvmeoAUYHsLK4FCkiU\""
    , "\"x7jHmEP1wTuvLTyPaQmK1tZMxT2Kwtuuzc5eTezAfgUGWv9CmMKVAoXXVaTPHsRHZ\""
    , "\"BAmNJaACngWLFTQPqZZdVoRu6nj14NZCzwoLRAJTxshtUKxSdwtLTgk7paZDjw2QB\""
    , "\"6BQPX2jXjy1Y27Deieh62VFA4zReoHuKtj72nUPsHXefkeNssAQxJ6yNk5VcEnHF2a\""
    , "\"4QhJfHm83g6gmHGoB6xMe97d89Y6VNwe6WNByc82BNPTYkVALbgYjNaGbdmxWamTv\""
    , "\"5hoM5M6FnLKGeZYRV6oj7Q8EvkA7SwaiSYiMKfS1c4yTjggwiW1QSpWTsGz9tF5WgU\""
    , "\"5r5gGWUHWqFLCJoKmqi1b4kuGnu3xpic2wHMdbg1neiVsTHSPsqbHRhW8C6hWnwhuR\""
    , "\"9YJ4qaRFK9ip8BQyN6Aibj3Mde6a8RUjKsYHtc6xDRLyYLXVrxGS1odLm8HHrj2bz\""
    , "\"5zAUebFNXECL36VBv19ujLcehmHhbvEsi3iUEM6GtbpzioWU2PmkohupzmT3H8S1qK\""
    , "\"eD6jPxkiF9rcdRX4PgdeUt2mGGMLfKiBb27LtMEq1r87A2AS35tVJ7gZik1Qr4jgh\""
    , "\"6evQBi67x9njzgQc96baFX2775oXDYjV4yLURqCoGgzx4fYzzGERghpLGwTqLeM6n\""
    , "\"ZwTbbGw4azbsjng6hoV52dMNcVye5Dyxe3DCBSCVKnphzTZNFfeqJNemnDwgf6pMr\""
    , "\"6Jjo1Suk9pf83yJLsPDdcjYN4izxuUWbggnTrNM6cMxswhHHXgASkm9mGqHGkRnCLe\""
    , "\"DTVPUeDBavRKd61zEKRpWZSjJuzNbzNiHAtf5gAs2Frhajp64CpHCUamHtnhwEJ6n\""
    , "\"6TVwVaDodQTLDXFizke68U6V45NauVy8bzz9h2Bq51U928iFcNdHQE4byw1ZZHj1Su\""
    , "\"riF2zRNCPyyHQNWEm4VBMtvezdAhv64tFzdK3QtAB7ngXPcBPKpuGYRtsH7CUuKQd\""
    , "\"7W9k4DNk88BHJPdYvc4CRMvST17YxskdANaUU6snC6f4guUFoJTF7GxdMHw7A8ABA\""
    , "\"5q7kt1Wh8E7EffqC8m4j6gevuHVGXtn8gVVPaNMbKaGtitcjkdWDqyCh7rttnp4ifa\""
    , "\"6GBTpEuFyPT2Erjx3ijHYVVbtmgL8HBKM919Mh6s7CsbQrfysETjCdSRNexwX3bXWa\""
    ]


signaturesBLS :: [Signature BLS]
signaturesBLS = map read
    [ "Signature \"sea7o326L9Yfo5UvdhZcKoPen7gEh7kwJPNuaUC3j5P1YUAr5MGC6koFWFMqRbjkGcLk5hBsTNSq3AHNzvDpai2EweLv5kMEWYqGaQbXQfz1xni38uSGkoKbh66yXa2MmuU\""
    , "Signature \"7XfyxouwEBADxA4P4uxemn1ryEhxnfQ8bze8Qpih1eXVSTCoMCXb9c52TSQHUmLqW9x9XrVtMPDZ5kcRBwxB5Wnhx8C94zi8WeUyqyNBwrTqwUFT58hpAfKaY6p761GxhVp\""
    , "Signature \"v1dyhPiJDKwZVm8MjcSt8imCaHnYf2q2a7eckdhUrd2CKC3RRqUDmGGGTp1msXKvHtGoyBKg7LGeaxhgastDhJDs4vLPjQW5Ux9y8PBP3BBXVBPepadRm8y1MffCRNTbYRe\""
    , "Signature \"4XRV8yTqVhirTFf8M5PPC1y1FMYcA14e9gBMeZ2HwjuAFMDyryicNywQopyUR8DLTJ8G2zq8kW1oRj5iu3USwtwqkCBAx57TEDW6rBNmPKZJZvZxX72xt2rJjVYwyLexgHy\""
    , "Signature \"3bhhLzUx6UvuJya6m93WZh17VK4tAAGdxLU5VQA3cgJXQcxtASpbNNNSBNtgHfGcUwjEJApzgN4pS5jRp9TT7QrKoKxM8S7gjuoEUkKo5aocXXc5a6wGQLg6N9Nh6phkzUd\""
    , "Signature \"mLtFhgiVgPTXnJTH4RDK6a2w2KsDctXLW5Cr7E8q5KPREJAj6HUBBW6u6gMw6Fo3gwywuNFVZFkmHw5U1onUTW8dDNgRB5pFzp6eZQZY8zJyM6TjCohE5Xq1ufoGaYaeg9e\""
    , "Signature \"uTbejXvVJgs5UD24izETvC9YU9GrpazVxcRkCipiYeMT5MowvnbeAYoJPrWdWLdn9kN8aCNLzSsGvqWeWpbmPwjoUu8Lt7NAsVXX8ZQNwKmCbJr62f1hUGKnjwzZfrb18Gz\""
    , "Signature \"tiJ6JmVXA1u9MXvs6gist9DNkNN9Ber9b41uNvJ4Zk439gihhuNPrsghCNvYd54kjeixDiSwkpbpBgSR37xrpiKtkCffvFgY6qTRccgK6KjbPQf4PaF5xDAHYmAgzAi9Vyg\""
    , "Signature \"tKhjeBuM8KSRrTLBmEoK5NvNKcch8tHsp1hkuAFRjTQ2fY3jHUwnQiwviZ1eRmagdSpGARvZdARGkFRX2a3nQzKyyrB5J9hsRbDubj5fbEHNG15JCWf4yvaYKvw8moBUSJJ\""
    , "Signature \"vRfAyDMjUSdSRiTCWP6r7jENyGN1X1UJuqKsvPVj7zgCjo1NexyuVoNtn9WWfs6Et3wFjz2GJ2TnNmcnpUPxnQpCf5HiPeShRAAJKRdjvVaoxkqhY2UkcBmZKsBa3nQSbv\""
    , "Signature \"oJyAQKo3W6oP2g222StiydrP5A2GZoFHaQhqDTsMSkeXCjA6gXgogrw7XEeQKAMiebsdk7P5PXAcFxcxQzRwWfYDiEH4qn3BTqdADrob54CtuaUN294MccqBc4HM6maWkPf\""
    , "Signature \"mwcMUHq9U9jKd8qnVAqqGRapXvwEWZBQdhxcZ4RVYKNRU4LqXkLhJHQAn3DUWf64W7kE36eKx3xDajCAGZCPJcQRqFP6vAfpDp8tooQSp8aP9dDNjkP1Gbb6u2MCbCBHbBA\""
    , "Signature \"9uAiYZmsqShJAmtU2n8GvxoK8PD89DiGC5oCGHGvzbh1SCMriMoopWQqdphMKEdpw3TL4W7uWFedR429piBSs1hwd7L5pTZYn5HwWCjNnSnHbMo7kxmPrVhSXja4ukKScNt\""
    , "Signature \"pcuDmaHKhujT8EwKxj4VC1GRj5ZH9WTmihyo5mTCFKJtaW9rGdLyn4dLm2MuUzZcYzUKfeMxFf7bZDpQ4rSqadGzea9NgzoaUJsBZPRMGgfWbo7NbvRaVd9u9pP7toJ6Tj5\""
    , "Signature \"8anmwm978FsEqrh8BcFS3TwXyCiA2a87NaYKTq5EsNsXyWkqTFT5Nsejte1vWCPZxWjSqoAEAd7XS76Riw2zCkkMPwT4ocasFrQDr58gtcW7vjTpnDUAAfJXf7knyQzdxS4\""
    , "Signature \"8VV8tTMSPcMX8sJyo1N4JYhrco6mvG9VbCrAdZXp2Cwx34bTFmHdp29NFVzHTAmEJHHrqakpFtUJa2Jfhr6ShHXgo6uv8XsTwWyRH8zVS4Wbp9aSMCpXkBFH8BYzScAsvw9\""
    , "Signature \"sDg5tBDyfLvMmTRzbgmBSFxt2zwRAbMWf5a9oBB6z1giZYz6kimB9eKxcN7KjzLXnnK5MdCvTfvCZVfd9fiVtVFuXFtnBW7BtByEuuwQb5FfBcqhjJA78PEkKpTh7zx53Kj\""
    , "Signature \"33EJPVykMw8PnQA94HVmm5pmJzR1fXccTsjD1oRSTUqFhGLDK7MvSPerzLVMfWsr5UkhCuRyLXyvj2hZSeodm1JZ6DNFeEbLmW6JPF2Gv8sFQXBMVpHsPd2btWtWic6EYph\""
    , "Signature \"7hoxBkcQgZAjEfNxJic5CYmjFtwS1bCivEAUUztVnjB9GTJDvBwCFHGH1encU2mkeHv9y5FKt2EuPrev68wT3H2MtJMigcCzMV2CTQGp1EzdEhmKjGLamVqMAZMA6vVpqqX\""
    , "Signature \"47JD4howGtDmyf7fPHZmDF88zFmmp6LcGK3aAjTEHSDXr8avdVvyRc9D1YgmVqz5JhJVH2vgyHRYjBpom349JFEruMZwUi8F5jgXG2CozBFFCobRvvhc7abDqsvak8K3NCA\""
    ]

