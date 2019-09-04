{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}

module HSChain.Types.Network where

import Codec.Serialise
import Control.Applicative

import Data.Char    (isDigit)
import Data.Bits    ((.|.), shiftL, shiftR)
import Data.Word    (Word8, Word16, Word32, byteSwap32)
import GHC.Generics (Generic)
import GHC.Read     (Read(..))

import qualified Data.Aeson                      as JSON
import qualified Data.List                       as List
import qualified Data.Text                       as Text
import qualified Text.ParserCombinators.ReadP    as ReadP
import qualified Text.ParserCombinators.ReadPrec as Read

type HostAddress = Word32
type HostAddress6 = (Word32, Word32, Word32, Word32)

-- | Converts 'HostAddress' to representation-independent IPv4 quadruple.
-- For example for @127.0.0.1@ the function will return @(0x7f, 0, 0, 1)@
-- regardless of host endianness.
--
{- -- prop> tow == hostAddressToTuple (tupleToHostAddress tow) -}
hostAddressToTuple :: HostAddress -> (Word8, Word8, Word8, Word8)
hostAddressToTuple ha' =
    let ha = htonl ha'
        byte i = fromIntegral (ha `shiftR` i) :: Word8
    in (byte 24, byte 16, byte 8, byte 0)

-- | Converts IPv4 quadruple to 'HostAddress'.
tupleToHostAddress :: (Word8, Word8, Word8, Word8) -> HostAddress
tupleToHostAddress (b3, b2, b1, b0) =
    let x `sl` i = fromIntegral x `shiftL` i :: Word32
    in ntohl $ (b3 `sl` 24) .|. (b2 `sl` 16) .|. (b1 `sl` 8) .|. (b0 `sl` 0)

#ifdef WORDS_LITTLEENDIAN
htonl :: Word32 -> Word32
htonl = byteSwap32

ntohl :: Word32 -> Word32
ntohl = byteSwap32
#endif

-- | Converts 'HostAddress6' to representation-independent IPv6 octuple.
--
{- -- prop> (w1,w2,w3,w4,w5,w6,w7,w8) == hostAddress6ToTuple (tupleToHostAddress6 (w1,w2,w3,w4,w5,w6,w7,w8)) -}
hostAddress6ToTuple :: HostAddress6 -> (Word16, Word16, Word16, Word16,
                                        Word16, Word16, Word16, Word16)
hostAddress6ToTuple (w3, w2, w1, w0) =
    let high, low :: Word32 -> Word16
        high w = fromIntegral (w `shiftR` 16)
        low w = fromIntegral w
    in (high w3, low w3, high w2, low w2, high w1, low w1, high w0, low w0)

-- | Converts IPv6 octuple to 'HostAddress6'.
tupleToHostAddress6 :: (Word16, Word16, Word16, Word16,
                        Word16, Word16, Word16, Word16) -> HostAddress6
tupleToHostAddress6 (w7, w6, w5, w4, w3, w2, w1, w0) =
    let add :: Word16 -> Word16 -> Word32
        high `add` low = (fromIntegral high `shiftL` 16) .|. (fromIntegral low)
    in (w7 `add` w6, w5 `add` w4, w3 `add` w2, w1 `add` w0)

data NetAddr = NetAddrV4 !HostAddress  !Word16
             | NetAddrV6 !HostAddress6 !Word16
          deriving (Eq, Ord, Generic)

instance Show NetAddr where
  show (NetAddrV4 ha p) = let (a,b,c,d) = hostAddressToTuple ha
                       in ((++show p) . (++":")) $ List.intercalate "." $ map show [a,b,c,d]
  show (NetAddrV6 ha p) = let (a,b,c,d,e,f,g,h) = hostAddress6ToTuple ha
                       in ((++show p) . (++".")) $ List.intercalate ":" $ map show [a,b,c,d,e,f,g,h]

instance Read NetAddr where
  readPrec
    = Read.lift $ optional (ReadP.string "tcp://") *> (readV4 <|> readV6)
    where
      readV4
        = NetAddrV4
       <$> (tupleToHostAddress <$>
             ((,,,) <$> digit <* ReadP.char '.'
                    <*> digit <* ReadP.char '.'
                    <*> digit <* ReadP.char '.'
                    <*> digit)
           )
       <*  ReadP.char ':'
       <*> digit
      --
      readV6
        = NetAddrV6
       <$> (tupleToHostAddress6 <$>
             ((,,,,,,,) <$> digit <* ReadP.char ':' <*> digit <* ReadP.char ':'
                        <*> digit <* ReadP.char ':' <*> digit <* ReadP.char ':'
                        <*> digit <* ReadP.char ':' <*> digit <* ReadP.char ':'
                        <*> digit <* ReadP.char ':' <*> digit)
           )
       <*  ReadP.char '.'
       <*> digit
      --
      digit :: Integral i => ReadP.ReadP i
      digit = fromInteger . read <$> ReadP.munch1 isDigit

instance Serialise NetAddr
instance JSON.ToJSON   NetAddr where
  toJSON netAddr = JSON.String $ Text.pack $ show netAddr
instance JSON.FromJSON NetAddr where
  parseJSON (JSON.String text) = case reads str of
    ((netaddr, ""):_) -> pure netaddr
    _                 -> fail $ "can't parse net addr from " ++ show str
    where
      str = Text.unpack text
  parseJSON _ = fail $ "NetAddr parsing expects String"

