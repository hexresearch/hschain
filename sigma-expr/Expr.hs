{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import qualified Data.ByteString         as BS
import qualified Data.ByteArray          as BA
import qualified Crypto.ECC.Edwards25519 as Ed
import qualified Crypto.Hash.Algorithms  as Hash
import qualified Crypto.Hash             as Hash
import Crypto.Error
----------------------------------------------------------------

-- | Secret possessed by prover
newtype Secret = Secret Ed.Scalar

-- | Value published by prover
newtype Share = Share { unShare :: Ed.Point }

-- | Proof of knowledge of discrete logarithm
data ProofDL = ProofDL
  { publicK     :: Share        --
  , commitmentA :: Share        --
  , responceZ   :: Ed.Scalar
  }

generateProof :: Secret -> IO ProofDL
generateProof secret@(Secret w) = do
  -- Choose commitment
  r <- Ed.scalarGenerate
  let a = Ed.toPoint r
  -- Compute challenge
  let ch :: Hash.Digest Hash.SHA256
      ch = Hash.hashFinalize
         $ flip Hash.hashUpdate (Ed.pointEncode a   :: BA.Bytes)
         $ flip Hash.hashUpdate (Ed.pointEncode pub :: BA.Bytes)
         $ Hash.hashInit

  let CryptoPassed e = Ed.scalarDecodeLong $ BS.tail $ BA.convert ch
      z              = r `Ed.scalarAdd` (w `Ed.scalarMul` e)
  return ProofDL
    { publicK     = Share pub
    , commitmentA = Share a
    , responceZ   = z
    }
  where
    pub = Ed.toPoint w


verifyProof :: ProofDL -> Bool
verifyProof ProofDL{..}
  = Ed.toPoint responceZ == (unShare commitmentA `Ed.pointAdd` (e `Ed.pointMul` unShare publicK))
  where
    -- Recompute challenge
    ch :: Hash.Digest Hash.SHA256
    ch = Hash.hashFinalize
       $ flip Hash.hashUpdate (Ed.pointEncode (unShare commitmentA) :: BA.Bytes)
       $ flip Hash.hashUpdate (Ed.pointEncode (unShare publicK)     :: BA.Bytes)
       $ Hash.hashInit
    CryptoPassed e = Ed.scalarDecodeLong $ BS.tail $ BA.convert ch


go = do
  sk <- Ed.scalarGenerate
  proof <- generateProof (Secret sk)
  print $ verifyProof proof
