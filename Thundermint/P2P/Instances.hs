-- | Some useful instances
--
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-deprecations #-}
module Thundermint.P2P.Instances where


import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Data.Monoid ((<>))
import Network.Socket


-- FIXME Add automatic (by Typable interface) serialization!!!
--
instance Serialise SockAddr where
    encode (SockAddrInet  (PortNum port) host) =
        encodeListLen 2 <> encodeWord 0 <> encode port <> encode host
    encode (SockAddrInet6 (PortNum port) flowInfo host scopeid) =
        encodeListLen 4 <> encodeWord 1 <> encode port <> encode flowInfo <> encode host <> encode scopeid
    encode _ = error "Serialise SockAddr not implemented yet!"
    decode = do
        len <- decodeListLen
        tag <- decodeWord
        case (len, tag) of
            (2, 0) -> SockAddrInet  <$> (PortNum <$> decode) <*> decode
            (4, 1) -> SockAddrInet6 <$> (PortNum <$> decode) <*> decode <*> decode <*> decode
            a      -> error ("Can't decode SocketAddr len/tag pair: " ++ show a)
