{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes         #-}
module HSChain.Network.Types
  ( -- * IP addresses and helpers
    NetAddr(..)
  , sockAddrToNetAddr
  , netAddrToSockAddr
    -- * API
  , NetworkAPI(..)
  , P2PConnection(..)
  , NetworkPort
    -- * Exceptions
  , NetworkError(..)
  ) where

import Codec.Serialise
import Control.Applicative
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Char    (isDigit)
import Data.Word    (Word16)
import GHC.Generics (Generic)
import GHC.Read     (Read(..))

import qualified Data.Aeson                      as JSON
import qualified Data.List                       as List
import qualified Data.Text                       as Text
import qualified Data.ByteString.Lazy            as LBS
import qualified Text.ParserCombinators.ReadP    as ReadP
import qualified Text.ParserCombinators.ReadPrec as Read
import Network.Socket ( hostAddressToTuple,  tupleToHostAddress
                      , hostAddress6ToTuple, tupleToHostAddress6
                      , HostAddress
                      , HostAddress6
                      , SockAddr(..)
                      , PortNumber
                      )


----------------------------------------------------------------
-- Network address
----------------------------------------------------------------

-- | Network address. It's distinct from 'NetAddr' from @network@
--   package in that it only support IP and could be serialised.
data NetAddr
  = NetAddrV4 !HostAddress  !Word16
  | NetAddrV6 !HostAddress6 !Word16
  deriving stock    (Eq, Ord, Generic)
  deriving anyclass (Serialise)

-- | Convert address from @network@ to serialisable address. Will
--   throw if address other than IP4\/6 is passed to it. But that
--   shouldn't be problem in practice.
sockAddrToNetAddr :: SockAddr -> NetAddr
sockAddrToNetAddr sa = case sa of
  SockAddrInet  port ha     -> NetAddrV4 ha $ fromIntegral port
  SockAddrInet6 port _ ha _ -> NetAddrV6 ha $ fromIntegral port
  _                         -> error $ "unsupported socket address kind: "++show sa

-- | Convert IP address to representation from @network@ library.
netAddrToSockAddr :: NetAddr -> SockAddr
netAddrToSockAddr (NetAddrV4 ha port) = SockAddrInet  (fromIntegral port)  ha
netAddrToSockAddr (NetAddrV6 ha port) = SockAddrInet6 (fromIntegral port) 0 ha 0

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


----------------------------------------------------------------
-- API
----------------------------------------------------------------

-- | Network port
type NetworkPort = PortNumber

-- | Dictionary with API for network programming. We use it to be able
--   to provide two implementations of networking. One is real network
--   and another is mock in-process network for testing.
data NetworkAPI = NetworkAPI
  { listenOn :: !(forall m. (MonadIO m, MonadMask m)
             => m (m (), m (P2PConnection, NetAddr)))
    -- ^ Start listening on given port. Returns action to stop listener
    --   and function for accepting new connections
  , connect  :: !(forall m. (MonadIO m, MonadThrow m, MonadMask m)
             => NetAddr -> m P2PConnection)
    -- ^ Connect to remote address
  , listenPort :: !NetworkPort
    -- ^ Listen port.
  }

data P2PConnection = P2PConnection
  { send          :: !(forall m. (MonadIO m) => LBS.ByteString -> m ())
    -- ^ Send data
  , recv          :: !(forall m. (MonadIO m) => m LBS.ByteString)
    -- ^ Receive data. Will throw exception if connection is closed
  , close         :: !(forall m. (MonadIO m) => m ())
  }

-- | Some network error.
data NetworkError
  = ConnectionTimedOut          -- ^ Connection timed out
  | NoAddressAvailable          -- ^ 
  | CantReverseLookipHostname   -- ^
  | ConnectionClosed            -- ^
  | ConnectionLoop
  | SelfConnection
  deriving stock    (Show)
  deriving anyclass (Exception)
