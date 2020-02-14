{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module HSChain.Network.Types where

import Codec.Serialise
import Control.Applicative
import Data.Char    (isDigit)
import Data.Word    (Word16)
import GHC.Generics (Generic)
import GHC.Read     (Read(..))

import qualified Data.Aeson                      as JSON
import qualified Data.List                       as List
import qualified Data.Text                       as Text
import qualified Text.ParserCombinators.ReadP    as ReadP
import qualified Text.ParserCombinators.ReadPrec as Read
import Network.Socket ( hostAddressToTuple,  tupleToHostAddress
                      , hostAddress6ToTuple, tupleToHostAddress6
                      , HostAddress
                      , HostAddress6
                      )


-- | Network address. It's distinct from adrress from @network@
--   package in that it only support IP and could be serialised.
data NetAddr
  = NetAddrV4 !HostAddress  !Word16
  | NetAddrV6 !HostAddress6 !Word16
  deriving stock    (Eq, Ord, Generic)
  deriving anyclass (Serialise)

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

instance JSON.ToJSON   NetAddr where
  toJSON netAddr = JSON.String $ Text.pack $ show netAddr
instance JSON.FromJSON NetAddr where
  parseJSON (JSON.String text) = case reads str of
    ((netaddr, ""):_) -> pure netaddr
    _                 -> fail $ "can't parse net addr from " ++ show str
    where
      str = Text.unpack text
  parseJSON _ = fail "NetAddr parsing expects String"

