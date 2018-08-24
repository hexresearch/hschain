{-# LANGUAGE RankNTypes #-}
-- |
-- Abstract API for network which support
module Thundermint.P2P.Types (
    NetworkAPI(..)
  , Connection(..)
  , NetworkError(..)
  , MockSocket(..)
  , MockNet(..)
  , HeaderSize
  ) where

import Control.Concurrent.STM

import Control.Exception      (Exception)
import Control.Monad.Catch    (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Map               (Map)

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket       as Net

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Dictionary with API for network programming. We use it to be able
--   to provide two implementations of networking. One is real network
--   and another is mock in-process network for testing.
data NetworkAPI addr = NetworkAPI
  { listenOn :: forall m. (MonadIO m, MonadThrow m, MonadMask m)
             => m (m (), m (Connection , addr))
    -- ^ Start listening on given port. Returns action to stop listener
    --   and function for accepting new connections
  , connect  :: forall m. (MonadIO m, MonadThrow m, MonadMask m)
             => addr -> m Connection
    -- ^ Connect to remote address
  }

data Connection = Connection
    { send  :: forall m. (MonadIO m) => LBS.ByteString -> m ()
      -- ^ Send data
    , recv  :: forall m. (MonadIO m) => m (Maybe LBS.ByteString)
      -- ^ Receive data
    , close :: forall m. (MonadIO m) => m ()
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
  { msckActive :: TVar Bool
  , msckSend   :: TChan LBS.ByteString
  , msckRecv   :: TChan LBS.ByteString
  }
  deriving (Eq)

-- | Mock network which uses STM to deliver messages
newtype MockNet addr = MockNet
  { mnetIncoming    :: TVar (Map (addr,Net.ServiceName)
                                 [(MockSocket, (addr,Net.ServiceName))])
    -- ^ Incoming connections for node.
  }
