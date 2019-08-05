{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Threshold (htf_thisModulesTests) where

import Test.Framework

import Control.Monad
import Control.Monad.Extra
import Crypto.Bls.PrivateKey as PrivateKey
import Crypto.Bls.PublicKey as PublicKey
import Crypto.Bls.Threshold as Threshold
import Crypto.Bls.Signature as Signature
import Crypto.Bls.Types
import Crypto.Bls.Util
import qualified Data.Vector as V


import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL


hexifyBs :: BS.ByteString -> BS.ByteString
hexifyBs = BSL.toStrict . BS.toLazyByteString . BS.byteStringHex


test_threshold_create_verify :: IO ()
test_threshold_create_verify = do
    let t = 7
        n = 9
    (secretKey, commi, secretFrags) <- Threshold.create t n
    assertBool =<< allM (\i -> Threshold.verifySecretFragment (i + 1) (secretFrags V.! i) commi t) [0..(n - 1)]


fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a


snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thr3 :: (a, b, c) -> c
thr3 (_, _, c) = c


test_threshold_create_sign_verify :: IO ()
test_threshold_create_sign_verify = do
    let t = 2
        n = 4
    -- Every player creates keys and fragments
    frags <- replicateM n (Threshold.create t n)
    let secretKeys  = V.fromList $ map fst3 frags
        publicFrags = V.fromList $ map snd3 frags
        secretFrags = V.fromList $ map thr3 frags
    -- TODO
    -- assertBool =<< allM (\i -> Threshold.verifySecretFragment (i + 1) (secretFrags V.! i) commi t) [0..(n - 1)]
    

    -- Create master public key
    let masterPublicKeyFrags = V.map V.head publicFrags
    masterPublicKey <- aggregateInsecurePublicKey masterPublicKeyFrags
    BS.putStrLn =<< (hexifyBs <$> (serializePublicKey masterPublicKey))

    --
    let rcvdFrags = V.generate n $ \i -> V.map (V.! i) secretFrags


    secretShares <- V.mapM aggregateInsecurePrivateKey rcvdFrags
    putStrLn "--- Secret Shares ---------"
    (flip V.imapM_) secretShares $ \i ssh -> do
        putStr ("  Player "  <> show i <> ": ")
        BS.putStrLn =<< (hexifyBs <$> (serializePrivateKey ssh))


    {-
    putStrLn "--- Secret Fragments ---------"
    (flip V.imapM_) secretFrags $ \i frs -> do
        putStrLn ("  Player "  <> show i <> ":")
        (flip V.imapM_) frs $ \j frk -> do
            putStr ("    " <> show j <> ") ")
            BS.putStrLn =<< (hexifyBs <$> (serializePrivateKey frk))

    putStrLn "--- Recvd Fragments ---------"
    (flip V.imapM_) rcvdFrags $ \i frs -> do
        putStrLn ("  Player "  <> show i <> ":")
        (flip V.imapM_) frs $ \j frk -> do
            putStr ("    " <> show j <> ") ")
            BS.putStrLn =<< (hexifyBs <$> (serializePrivateKey frk))
    -}

    --
    let message = ("My Test Message" :: BS.ByteString)
    hash <- hash256 message
    BS.putStr "Hash message: "
    BS.putStrLn (hexifyBs $ unHash256 hash)

    --
    sigU1 <- signInsecure (secretShares V.! 0) message
    sigU3 <- signInsecure (secretShares V.! 2) message

    sig <- aggregateUnitSigs (V.fromList [sigU1, sigU3]) message [1, 3]

    r <- verifyInsecure sig (V.fromList [hash]) (V.fromList [masterPublicKey])
    putStrLn "Result:"
    print r

    return ()





-- ********************************8


    {-
    putStrLn "--HASKELL PRIVATE KEYS--"
    BS.putStr   "  Secret Key: "
    bsSecretKey <- PrivateKey.serializePrivateKey secretKey
    BS.putStrLn (hexifyBs bsSecretKey)
    BS.putStrLn   "  Secret Frags: "
    forM_ (zip (V.toList secretFrags) [1..]) $ \(sf, i) -> do
        putStr ("    " ++ show i ++ ") ")
        bssf <- PrivateKey.serializePrivateKey sf
        BS.putStrLn (hexifyBs bssf)
    BS.putStrLn   "  Commitments: "
    forM_ (zip (V.toList commi) [1..]) $ \(sf, i) -> do
        putStr ("    " ++ show i ++ ") ")
        bssf <- PublicKey.serializePublicKey sf
        BS.putStrLn (hexifyBs bssf)
    putStrLn "--AFTER--"
    -}

    {-
    // Участники обмениваются друг с другом секретными фрагментами
    // и проверяют их
    for(size_t player_source = 1; player_source <= N; ++player_source) // NB: смотри индексы!
        for(size_t player_target = 1; player_target <= N; ++player_target) // NB: смотри индексы!
            assert(bls::Threshold::VerifySecretFragment(
                        player_target,
                        p_secretFragments[player_source-1][player_target-1], p_commitments[player_source-1], T));
                        -}


