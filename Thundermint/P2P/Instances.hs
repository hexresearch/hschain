-- | Some useful instances
--
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-deprecations #-}
module Thundermint.P2P.Instances where


import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Data.Aeson (FromJSON, ToJSON, ToJSONKey, Value(..), parseJSON, toJSON, toJSONKey)
import Data.Aeson.Types (FromJSONKey, FromJSONKeyFunction(..), fromJSONKey, toJSONKeyText, typeMismatch)
import Data.Monoid ((<>))
import Network.Socket
import qualified Data.Text             as T
import Data.Maybe                (fromMaybe)
import System.IO.Unsafe          (unsafePerformIO)

import Thundermint.P2P.Consts


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



instance ToJSON SockAddr where
  toJSON = String . sa2Text


instance ToJSONKey SockAddr where
  toJSONKey = toJSONKeyText sa2Text


instance FromJSONKey SockAddr where
  fromJSONKey = FromJSONKeyText text2Sa


instance FromJSON SockAddr where
  parseJSON (String s) = return $ text2Sa s
  parseJSON invalid    = typeMismatch "SockAddr" invalid


sa2Text :: SockAddr -> T.Text
sa2Text sa = T.pack
        $ fromMaybe "" mHN <> maybe "" (":"<>) mSN
     where (mHN, mSN) = unsafePerformIO $ getNameInfo [] True True sa


text2Sa :: T.Text -> SockAddr
text2Sa s = addrAddress $ head addrInfos
  where (hN, sN)  = T.breakOn ":" s
        mHN       = if T.null hN
                      then Nothing
                      else Just $ T.unpack hN
        mSN       = if T.null sN
                       then Just thundermintPort
                       else Just $ T.unpack $ T.tail sN
        addrInfos = unsafePerformIO $ getAddrInfo Nothing mHN mSN

