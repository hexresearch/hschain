{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Abstract API for network which support
module Thundermint.P2P.Types (
    NetworkAPI(..)
  , NetAddr(..)
  , P2PConnection(..)
  , NetworkError(..)
  , MockSocket(..)
  , MockNet(..)
  , HeaderSize
  , RecvFun
  , NetworkPort
  , PeerId
  , PeerInfo(..)
  ) where

import Codec.Serialise
import Control.Applicative
import Control.Concurrent.STM
import Control.Exception        (Exception)
import Control.Monad.Catch      (MonadMask, MonadThrow)
import Control.Monad.IO.Class   (MonadIO)
import Data.ByteString.Internal (ByteString(..))
import qualified Data.List as List
import Data.Map                 (Map)
import Data.Set                 (Set)
import Data.Word
import GHC.Generics             (Generic)
import Network.Socket           (SockAddr)


import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket       as Net

----------------------------------------------------------------
--
----------------------------------------------------------------

type PeerId = Word64


data PeerInfo addr = PeerInfo
    { piPeerId   :: !PeerId
    , piPeerPort :: !Word32
    } deriving (Show, Generic)


instance Serialise (PeerInfo addr)


----------------------------------------------------------------
--
----------------------------------------------------------------

data NetAddr = NetAddrV4 !Net.HostAddress !Net.PortNumber
          | NetAddrV6 !Net.HostAddress6 !Net.PortNumber
          deriving (Eq, Ord)

instance Show NetAddr where
  show (NetAddrV4 ha p) = let (a,b,c,d) = Net.hostAddressToTuple ha
                       in ((++show p) . (++":")) $ List.intercalate "." $ map show [a,b,c,d]
  show (NetAddrV6 ha p) = let (a,b,c,d,e,f,g,h) = Net.hostAddress6ToTuple ha
                       in ((++show p) . (++".")) $ List.intercalate ":" $ map show [a,b,c,d,e,f,g,h]

newtype P a = P { parse :: String -> [(a, String)] }

instance Functor P where
  fmap f (P q) = P $ \s -> [(f a, s') | (a,s') <- q s]

instance Applicative P where
  pure c = P $ \s -> [(c, s)]
  P pf <*> P pa = P $ \s -> [(f a, s'') | (f, s') <- pf s, (a, s'') <- pa s']

instance Alternative P where
  empty = P $ const []
  P f <|> P g = P $ \s -> f s ++ g s


pcheck :: (a -> [b]) -> P a -> P b
pcheck f (P q) = P $ \s -> [(y, s') | (x, s') <- q s, y <- f x]

sepBy1 :: P d -> P a -> P [a]
sepBy1 delim p = (:) <$> p <*> many (delim *> p)

pstring :: String -> P String
pstring [] = pure []
pstring (c:cs) = (:) <$> pitem c <*> pstring cs

pitem :: Char -> P Char
pitem x = P $ \s -> case s of
  (i:is) | i == x -> [(x, is)]
  _               -> []

pnum :: Integral i => P i
pnum = fromIntegral <$> pinteger
  where
    pinteger :: P Integer
    pinteger = read <$> (pstring "0" <|> nonzero)
    nonzero = (:) <$> foldr (<|>) empty (map pitem "123456789") <*> many (foldr (<|>) empty $ map pitem "0123456789")

instance Read NetAddr where
  readsPrec _ = parse (readV4 <|> readV6)
    where
      readV4 = NetAddrV4 <$> (Net.tupleToHostAddress <$> (pcheck getTuple4 $ sepBy1 (pitem '.') pnum)) <* pstring ":" <*> pnum
      readV6 = NetAddrV6 <$> (Net.tupleToHostAddress6 <$> (pcheck getTuple6 $ sepBy1 (pitem ':') pnum)) <* pstring "." <*> pnum
      getTuple4 [a,b,c,d] = [(a,b,c,d)]
      getTuple4 _         = []
      getTuple6 [a,b,c,d,e,f,g,h] = [(a,b,c,d,e,f,g,h)]
      getTuple6 _                 = []

-- | Network port
type NetworkPort = Net.PortNumber

-- | Dictionary with API for network programming. We use it to be able
--   to provide two implementations of networking. One is real network
--   and another is mock in-process network for testing.
data NetworkAPI = NetworkAPI
  { listenOn :: !(forall m. (MonadIO m, MonadThrow m, MonadMask m)
             => m (m (), m (P2PConnection, SockAddr)))
    -- ^ Start listening on given port. Returns action to stop listener
    --   and function for accepting new connections
  , connect  :: !(forall m. (MonadIO m, MonadThrow m, MonadMask m)
             => SockAddr -> m P2PConnection)
    -- ^ Connect to remote address
  , filterOutOwnAddresses :: !(forall m. (MonadIO m, MonadThrow m, MonadMask m)
             => Set SockAddr -> m (Set SockAddr))
    -- ^ Filter out local addresses of node. Batch processing for speed.
  , normalizeNodeAddress :: !(SockAddr -> Maybe NetworkPort -> SockAddr)
    -- ^ Normalize address, for example, convert '20.15.10.20:24431' to '20.15.10.20:50000'
  , listenPort :: !NetworkPort 
  }

data P2PConnection = P2PConnection
  { send  :: !(forall m. (MonadIO m) => LBS.ByteString -> m ())
    -- ^ Send data
  , recv  :: !(forall m. (MonadIO m) => m (Maybe LBS.ByteString))
    -- ^ Receive data
  , close :: !(forall m. (MonadIO m) => m ())
    -- ^ Close socket
  }

type HeaderSize = Int


data NetworkError = ConnectionTimedOut
                  | NoAddressAvailable
  deriving (Show)

instance Exception NetworkError


----------------------------------------------------------------
-- Mock network
----------------------------------------------------------------

-- | Sockets for mock network
data MockSocket = MockSocket
  { msckActive :: !(TVar Bool)
  , msckSend   :: !(TChan LBS.ByteString)
  , msckRecv   :: !(TChan LBS.ByteString)
  }
  deriving (Eq)

-- | Mock network which uses STM to deliver messages
newtype MockNet addr = MockNet
  { mnetIncoming    :: TVar (Map (addr,Net.ServiceName)
                                 [(MockSocket, (addr,Net.ServiceName))])
    -- ^ Incoming connections for node.
  }



----------------------------------------------------------------
-- types used in tls
----------------------------------------------------------------

-- | Type for the action to receive input data
type RecvFun = IO ByteString
