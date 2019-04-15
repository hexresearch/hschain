{-# LANGUAGE BinaryLiterals #-}
module Thundermint.Types.BitVector
    ( FixedBitVector
    , new
    , insert
    , toList
    , size
    )  where


import Control.DeepSeq
import Data.Bits
import Data.Word
import GHC.Exts hiding (toList)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM


-- | Bitvector with fixed size
data FixedBitVector = SmallBitVector {-# UNPACK #-} !Int {-# UNPACK #-} !Word64
                    | BigBitVector   {-# UNPACK #-} !Int {-# UNPACK #-} !(V.Vector Word64)


instance NFData FixedBitVector where
    rnf x = x `seq` ()


new :: Int -> FixedBitVector
new vsize
    | vsize <= 64 = SmallBitVector vsize 0
    | otherwise  = BigBitVector vsize (V.replicate ((vsize + 64 - 1) `unsafeShiftR` 6) 0)


insert :: Int -> FixedBitVector -> FixedBitVector
insert idx (SmallBitVector vsize w)
    | (idx < 0) || (idx >= vsize) = error ("Bounds checking problem, idx = " ++ show idx ++ ", while size = " ++ show vsize)
    | otherwise = SmallBitVector vsize (w `setBit` idx)
insert idx (BigBitVector vsize v)
    | (idx < 0) || (idx >= vsize) = error ("Bounds checking problem, idx = " ++ show idx ++ ", while size = " ++ show vsize)
    | otherwise =
        let idx' = idx `unsafeShiftR` 6
        in (BigBitVector vsize (V.modify (\v' -> VM.unsafeModify v' (\e -> e `setBit` (idx .&. 0b111111)) idx') v))
{-# INLINE insert #-}


size :: FixedBitVector -> Int
size (SmallBitVector vsize _) = vsize
size (BigBitVector vsize _)   = vsize

-- Start of code from Data.IntSet.Internal ----


shiftRL, shiftLL :: Word64 -> Int -> Word64
shiftRL = unsafeShiftR
shiftLL = unsafeShiftL


indexOfTheOnlyBit :: Word64 -> Int
indexOfTheOnlyBit bitmask =
  I# (lsbArray `indexInt8OffAddr#` unboxInt (intFromWord64 ((bitmask * magic) `shiftRL` offset)))
  where unboxInt (I# i) = i
        magic = 0x07EDD5E59A4E28C2
        offset = 58
        !lsbArray = "\63\0\58\1\59\47\53\2\60\39\48\27\54\33\42\3\61\51\37\40\49\18\28\20\55\30\34\11\43\14\22\4\62\57\46\52\38\26\32\41\50\36\17\19\29\10\13\21\56\45\25\31\35\16\9\12\44\24\15\8\23\7\6\5"#
        -- The lsbArray gets inlined to every call site of indexOfTheOnlyBit.
        -- That cannot be easily avoided, as GHC forbids top-level Addr# literal.
        -- One could go around that by supplying getLsbArray :: () -> Addr# marked
        -- as NOINLINE. But the code size of calling it and processing the result
        -- is 48B on 32-bit and 56B on 64-bit architectures -- so the 32B and 64B array
        -- is actually improvement on 32-bit and only a 8B size increase on 64-bit.
{-# INLINE indexOfTheOnlyBit #-}


intFromWord64 :: Word64 -> Int
intFromWord64 w = fromIntegral w
{-# INLINE intFromWord64 #-}


lowestBitMask :: Word64 -> Word64
lowestBitMask x = x .&. negate x
{-# INLINE lowestBitMask #-}


foldrBits :: (Int -> a -> a) -> a -> Word64 -> a
foldrBits f z bitmap = go (revWord64 bitmap) z
  where go 0 acc = acc
        go bm acc = go (bm `xor` bitmask) ((f $! (63-bi)) acc)
          where !bitmask = lowestBitMask bm
                !bi = indexOfTheOnlyBit bitmask
        {-. INLINE go #-}
{-# INLINE foldrBits #-}


-- Reverse the order of bits in the Word64.
revWord64 :: Word64 -> Word64
revWord64 x1 =
    case ((x1 `shiftRL` 1) .&. 0x5555555555555555) .|. ((x1 .&. 0x5555555555555555) `shiftLL` 1) of
              x2 -> case ((x2 `shiftRL` 2) .&. 0x3333333333333333) .|. ((x2 .&. 0x3333333333333333) `shiftLL` 2) of
                 x3 -> case ((x3 `shiftRL` 4) .&. 0x0F0F0F0F0F0F0F0F) .|. ((x3 .&. 0x0F0F0F0F0F0F0F0F) `shiftLL` 4) of
                   x4 -> case ((x4 `shiftRL` 8) .&. 0x00FF00FF00FF00FF) .|. ((x4 .&. 0x00FF00FF00FF00FF) `shiftLL` 8) of
                     x5 -> case ((x5 `shiftRL` 16) .&. 0x0000FFFF0000FFFF) .|. ((x5 .&. 0x0000FFFF0000FFFF) `shiftLL` 16) of
                       x6 -> ( x6 `shiftRL` 32             ) .|. ( x6               `shiftLL` 32);
{-# INLINE revWord64 #-}


-- End of code from Data.IntSet.Internal ----


toList :: FixedBitVector -> [Int]
toList (SmallBitVector _size w) = build (\f acc -> foldrBits f acc w)
toList (BigBitVector   _size vs) =
    V.ifoldr' (\idx w lst ->
            let s = idx `unsafeShiftL` 6
            in augment (\f acc -> foldrBits (f . (s+)) acc w) lst)
        []
        vs
{-# INLINE toList #-}
