{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crypto.Bls.Arrays where


import qualified Data.Vector as V
import Foreign.Ptr (Ptr, plusPtr)
import Data.Proxy ( Proxy(..) )
import Foreign.Marshal.Alloc ( allocaBytes )
import Control.Exception (bracket_)
import Data.Functor ( ($>) )
import Foreign.C.Types (CSize)

import Crypto.Bls.Types
import Crypto.Bls.Internal


-- | Perform an action with a temporary pointer to an array of values
--
-- The input values are placed consecutively in memory using the 'PlacementNew'
-- mechanism.
--
-- This function is intended for types which are not managed by the Haskell
-- runtime, but by a foreign system (such as C).
--
-- The pointer is not guaranteed to be usuable outside the scope of this
-- function. The same warnings apply as for 'withForeignPtr'.
withArrayPtrLen
    :: forall a b
     . (WithPtr a, CSizeOf (C a), PlacementNew (C a))
    => V.Vector a
    -> (Ptr (C a) -> CSize -> IO b)
       -- ^ Computation to perform on array.
    -> IO (Maybe b)
       -- ^ With empty input vector the action can not be applied and
       -- the result will be Nothing.
withArrayPtrLen arr act
    | V.null arr = pure Nothing
    | otherwise =
        allocaBytes arraySize $ \arrPtr ->
          bracket_
            (V.foldM'_ copyNext arrPtr arr)
            (deconstructArray arrPtr )
            (Just <$> act arrPtr (fromIntegral $ V.length arr))
  where
    elemSize = cSizeOf (Proxy :: Proxy (C a))
    arraySize = elemSize * V.length arr

    copyNext :: Ptr (C a) -> a -> IO (Ptr (C a))
    copyNext !ptr obj = copyObj ptr obj $> plusPtr ptr elemSize

    copyObj :: Ptr (C a) -> a -> IO ()
    copyObj dstPtr src =
        withPtr src $ \srcPtr ->
          placementNew srcPtr dstPtr

    deconstructArray :: Ptr (C a) -> IO ()
    deconstructArray !begin = deconstructNext begin
      where
        deconstructNext !ptr
            | ptr == end = pure ()
            | otherwise = do placementDelete ptr
                             deconstructNext $ ptr `plusPtr` elemSize

        end :: Ptr (C a)
        end = begin `plusPtr` arraySize

