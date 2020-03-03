{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Crypto.Bls.CPP.Threshold
    ( Threshold
    , thresholdCreate
    , thresholdVerifySecretFragment
    , thresholdAggregateUnitSigs
    ) where


import Data.ByteString (ByteString)
import Data.Maybe
import Data.Vector (Vector)
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr (Ptr)
import qualified Data.Vector as V
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Crypto.Bls.CPP.Arrays
import Crypto.Bls.CPP.Internal
import Crypto.Bls.CPP.Types


C.context blsCtx
C.include "chiabls/bls.hpp"
C.include "<vector>"
C.include "<iostream>"
C.using "namespace bls"
C.using "namespace std"


thresholdCreate :: Int
       -> Int
       -> IO (PrivateKey, Vector PublicKey, Vector PrivateKey)
thresholdCreate t n =
    allocaArray n $ \(haskCommitment :: Ptr (Ptr C'PublicKey)) ->
        allocaArray n $ \(haskSecretFragments :: Ptr (Ptr C'PrivateKey)) -> do
            secretKey <- fromPtr [C.block| PrivateKey * {
                size_t const T = $(size_t ti);
                size_t const N = $(size_t ni);
                std::vector<bls::PublicKey>  commitments;
                std::vector<bls::PrivateKey> secret_fragments;
                for(size_t i = 0; i < N; ++i)
                {
                    g1_t g;
                    commitments.emplace_back(PublicKey::FromG1(&g));
                    bn_t b;
                    bn_new(b);
                    secret_fragments.emplace_back(bls::PrivateKey::FromBN(b));
                }
                PrivateKey secret_key = Threshold::Create(commitments, secret_fragments, T, N);
                for(size_t i = 0; i < N; ++i)
                {
                    $(PublicKey * * haskCommitment)[i]       = new PublicKey(commitments[i]);
                    $(PrivateKey * * haskSecretFragments)[i] = new PrivateKey(secret_fragments[i]);
                }
                return new PrivateKey(secret_key);
                }|]
            commitment       <- peekArray n haskCommitment      >>= fmap V.fromList . mapM (fromPtr . pure)
            secrectFragments <- peekArray n haskSecretFragments >>= fmap V.fromList . mapM (fromPtr . pure)
            return (secretKey, commitment, secrectFragments)
  where
    ti :: CSize
    ti = fromIntegral t
    ni :: CSize
    ni = fromIntegral n


thresholdVerifySecretFragment :: Int
                              -> PrivateKey
                              -> Vector PublicKey
                              -> Int
                              -> IO Bool
thresholdVerifySecretFragment player secretFragment commitment t =
    fmap (maybe False toBool) $
        withPtr secretFragment $ \ptrSecretFragment ->
            withArrayPtrLen commitment $ \ptrCommitment lenCommitment ->
                [C.exp| bool {
                    Threshold::VerifySecretFragment(
                          $(size_t c'player)
                        , *$(PrivateKey * ptrSecretFragment)
                        , std::vector<PublicKey>( $(PublicKey * ptrCommitment)
                                                , $(PublicKey * ptrCommitment) + $(size_t lenCommitment))
                        , $(size_t c'T))
                }|]
  where
    c'player           = fromIntegral player
    c'T                = fromIntegral t


thresholdAggregateUnitSigs :: Vector InsecureSignature
                           -> ByteString
                           -> [Int]
                           -> IO InsecureSignature
thresholdAggregateUnitSigs sigs message players =
    fmap (fromMaybe (error "empty sigs array")) $
    withArrayLen (map fromIntegral players) $ \t ptrPlayers ->
        let t' = fromIntegral t in
        withArrayPtrLen sigs $ \ptrSigs lenSigs ->
            fromPtr [C.exp| InsecureSignature * {
                new InsecureSignature(Threshold::AggregateUnitSigs(
                    std::vector<InsecureSignature>( $(InsecureSignature * ptrSigs)
                                                  , $(InsecureSignature * ptrSigs) + $(size_t lenSigs)),
                    (const uint8_t *)($bs-ptr:message),
                    $bs-len:message,
                    ($(size_t * ptrPlayers)),
                    $(size_t t')
                    ))
            }|]

