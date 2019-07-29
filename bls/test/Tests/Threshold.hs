{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Threshold (htf_thisModulesTests) where

import Test.Framework

-- import Crypto.Bls
import Crypto.Bls.Threshold as Threshold
import qualified Data.Vector as V

test_threshold_create :: IO ()
test_threshold_create = do
    let t = 7
        n = 9
    putStrLn "--BEFORE--"
    (_secretKey, commi, secretFrags) <- Threshold.create t n
    putStrLn "--AFTER--"
    r <- Threshold.verifySecretFragment 1 (secretFrags V.! 0) commi t
    print r


    {-
    // Участники обмениваются друг с другом секретными фрагментами
    // и проверяют их
    for(size_t player_source = 1; player_source <= N; ++player_source) // NB: смотри индексы!
        for(size_t player_target = 1; player_target <= N; ++player_target) // NB: смотри индексы!
            assert(bls::Threshold::VerifySecretFragment(
                        player_target,
                        p_secretFragments[player_source-1][player_target-1], p_commitments[player_source-1], T));
                        -}


