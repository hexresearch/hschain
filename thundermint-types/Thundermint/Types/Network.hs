{-# LANGUAGE DeriveGeneric #-}

module Thundermint.Types.Network where

import Codec.Serialise
import Control.Applicative

import Data.Char    (isDigit)
import Data.Word    (Word16)
import GHC.Generics (Generic)
import GHC.Read     (Read(..))

import qualified Data.Aeson                      as JSON
import qualified Data.List                       as List
import qualified Data.Text                       as Text
import qualified Network.Socket                  as Net
import qualified Text.ParserCombinators.ReadP    as ReadP
import qualified Text.ParserCombinators.ReadPrec as Read

data NetAddr = NetAddrV4 !Net.HostAddress  !Word16
             | NetAddrV6 !Net.HostAddress6 !Word16
          deriving (Eq, Ord, Generic)

instance Show NetAddr where
  show (NetAddrV4 ha p) = let (a,b,c,d) = Net.hostAddressToTuple ha
                       in ((++show p) . (++":")) $ List.intercalate "." $ map show [a,b,c,d]
  show (NetAddrV6 ha p) = let (a,b,c,d,e,f,g,h) = Net.hostAddress6ToTuple ha
                       in ((++show p) . (++".")) $ List.intercalate ":" $ map show [a,b,c,d,e,f,g,h]

instance Read NetAddr where
  readPrec
    = Read.lift $ optional (ReadP.string "tcp://") *> (readV4 <|> readV6)
    where
      readV4
        = NetAddrV4
       <$> (Net.tupleToHostAddress <$>
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
       <$> (Net.tupleToHostAddress6 <$>
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

sockAddrToNetAddr :: Net.SockAddr -> NetAddr
sockAddrToNetAddr sa = case sa of
  Net.SockAddrInet  port ha     -> NetAddrV4 ha $ fromIntegral port
  Net.SockAddrInet6 port _ ha _ -> NetAddrV6 ha $ fromIntegral port
  _                             -> error $ "unsupported socket address kind: "++show sa

netAddrToSockAddr :: NetAddr -> Net.SockAddr
netAddrToSockAddr (NetAddrV4 ha port) = Net.SockAddrInet  (fromIntegral port)  ha
netAddrToSockAddr (NetAddrV6 ha port) = Net.SockAddrInet6 (fromIntegral port) 0 ha 0

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

