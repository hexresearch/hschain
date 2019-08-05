{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Crypto.Bls.Types
    {- ( PrivateKey
    , PublicKey
    ) -} where

import Foreign.ForeignPtr
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Types  as C
import qualified Language.C.Inline.Context as C

import Control.Exception (mask_)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import Foreign.Ptr (Ptr, FunPtr)

import Crypto.Bls.Internal


C.context blsCtx
C.include "bls.hpp"
C.using "namespace bls"



-- | Types of which a value can be constructed from a pointer to the C
-- equivalent of that value
--
-- Used to wrap values created in C.
class FromPtr a where
    fromPtr :: IO (Ptr (C a)) -> IO a

-- | Perform an IO action with a pointer to the C equivalent of a value
class WithPtr a where
    -- | Perform an action with a temporary pointer to the underlying
    -- representation of @a@
    --
    -- The pointer is not guaranteed to be usuable outside the scope of this
    -- function. The same warnings apply as for 'withForeignPtr'.
    withPtr :: a -> (Ptr (C a) -> IO b) -> IO b


-- * PrivateKey ---------------------------------------------------------------

newtype PrivateKey = PrivateKey { unPrivateKey :: ForeignPtr C'PrivateKey }

type instance C PrivateKey = C'PrivateKey

mkFinalizer "deletePrivateKey" "bls::PrivateKey" ''C'PrivateKey

mkPlacementNewInstance ''PrivateKey

instance FromPtr PrivateKey where
    fromPtr = objFromPtr PrivateKey deletePrivateKey

instance WithPtr PrivateKey where withPtr = withForeignPtr . unPrivateKey

instance CSizeOf C'PrivateKey where cSizeOf _proxy = fromIntegral [C.pure| size_t { sizeof(bls::PrivateKey) }|]


-- * PublicKey ----------------------------------------------------------------

newtype PublicKey = PublicKey { unPublicKey :: ForeignPtr C'PublicKey }

type instance C PublicKey = C'PublicKey

mkFinalizer "deletePublicKey" "bls::PublicKey" ''C'PublicKey

mkPlacementNewInstance ''PublicKey

instance FromPtr PublicKey where
    fromPtr = objFromPtr PublicKey deletePublicKey

instance WithPtr PublicKey where withPtr = withForeignPtr . unPublicKey

instance CSizeOf C'PublicKey where cSizeOf _proxy = fromIntegral [C.pure| size_t { sizeof(bls::PublicKey) }|]


-- * Signature ----------------------------------------------------------------

newtype Signature = Signature { unSignature :: ForeignPtr C'Signature }

type instance C Signature = C'Signature

mkFinalizer "deleteSignature" "bls::Signature" ''C'Signature

mkPlacementNewInstance ''Signature

instance FromPtr Signature where
    fromPtr = objFromPtr Signature deleteSignature

instance WithPtr Signature where withPtr = withForeignPtr . unSignature


-- * InsecureSignature --------------------------------------------------------

newtype InsecureSignature = InsecureSignature { unInsecureSignature :: ForeignPtr C'InsecureSignature }

type instance C InsecureSignature = C'InsecureSignature

mkFinalizer "deleteInsecureSignature" "bls::InsecureSignature" ''C'InsecureSignature

mkPlacementNewInstance ''InsecureSignature

instance FromPtr InsecureSignature where
    fromPtr = objFromPtr InsecureSignature deleteInsecureSignature

instance WithPtr InsecureSignature where withPtr = withForeignPtr . unInsecureSignature

instance CSizeOf C'InsecureSignature where cSizeOf _proxy = fromIntegral [C.pure| size_t { sizeof(bls::InsecureSignature) }|]

-- * AggregationInfo ----------------------------------------------------------

newtype AggregationInfo = AggregationInfo { unAggregationInfo :: ForeignPtr C'AggregationInfo }

type instance C AggregationInfo = C'AggregationInfo

mkFinalizer "deleteAggregationInfo" "bls::AggregationInfo" ''C'AggregationInfo

mkPlacementNewInstance ''AggregationInfo

instance FromPtr AggregationInfo where
    fromPtr = objFromPtr AggregationInfo deleteAggregationInfo

instance WithPtr AggregationInfo where withPtr = withForeignPtr . unAggregationInfo


-- * Threshold ----------------------------------------------------------------

newtype Threshold = Threshold { unThreshold :: ForeignPtr C'Threshold }

type instance C Threshold = C'Threshold

mkFinalizer "deleteThreshold" "bls::Threshold" ''C'Threshold

mkPlacementNewInstance ''Threshold

instance FromPtr Threshold where
    fromPtr = objFromPtr Threshold deleteThreshold

instance WithPtr Threshold where withPtr = withForeignPtr . unThreshold


-- * Hash256 ----------------------------------------------------------------

newtype Hash256 = Hash256 { unHash256 :: ByteString } -- TODO use ShortByteString


