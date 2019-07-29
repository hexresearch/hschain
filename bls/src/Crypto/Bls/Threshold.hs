{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Crypto.Bls.Threshold
    ( Threshold
    , create
    , verifySecretFragment
    ) where


import Data.Vector (Vector)
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr (Ptr)
import qualified Data.Vector as V
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C

import Crypto.Bls.Arrays
import Crypto.Bls.Internal
import Crypto.Bls.Types


C.context blsCtx
C.include "bls.hpp"
C.include "<vector>"
C.include "<iostream>"
C.using "namespace bls"
C.using "namespace std"


create :: Int
       -> Int
       -> IO (PrivateKey, Vector PublicKey, Vector PrivateKey)
create t n =
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
                // now copy to Haskell arrays
                for(size_t i = 0; i < N; ++i)
                {
                    $(PublicKey * * haskCommitment)[i]       = new PublicKey(commitments[i]);
                    $(PrivateKey * * haskSecretFragments)[i] = new PrivateKey(secret_fragments[i]);
                }
                //
                // PublicKeys:
                cout << "----- commitment:\n";
                uint8_t pkbytes[bls::PublicKey::PUBLIC_KEY_SIZE];
                for(size_t i = 0; i < N; ++i)
                {
                    cout << "PK " << i << " -------------------\n";
                    $(PublicKey * * haskCommitment)[i]->Serialize(pkbytes);
                    for (auto b: pkbytes) cout << "0x" << hex << (int)b << " ";
                    cout << dec << "\n";
                }
                cout << "----- done\n";
                // ok
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


verifySecretFragment :: Int
                     -> PrivateKey
                     -> Vector PublicKey
                     -> Int
                     -> IO Bool
verifySecretFragment player secretFragment commitment t =
    fmap (maybe False toBool) $
        withPtr secretFragment $ \ptrSecretFragment ->
            withArrayPtr commitment $ \ptrCommitment ->
                [C.block| bool {

                    cout << "----- commitment 2:\n";
                    uint8_t pkbytes[bls::PublicKey::PUBLIC_KEY_SIZE];
                    for(size_t i = 0; i < $(int32_t c'commitmentLength); ++i)
                    {
                        cout << "PK " << i << " -------------------\n";
                        $(PublicKey * ptrCommitment)[i].Serialize(pkbytes);
                        for (auto b: pkbytes) cout << "0x" << hex << (int)b << " ";
                        cout << dec << "\n";
                    }
                    cout << "----- done\n";


                    return Threshold::VerifySecretFragment(
                          $(size_t c'player)
                        , *$(PrivateKey * ptrSecretFragment)
                        , std::vector<PublicKey>( $(PublicKey * ptrCommitment)
                                                , $(PublicKey * ptrCommitment) + $(int32_t c'commitmentLength))
                        , $(size_t c'T)
                        );

                }|]
  where
    c'player           = fromIntegral player
    c'T                = fromIntegral t
    c'commitmentLength = fromIntegral $ V.length commitment



