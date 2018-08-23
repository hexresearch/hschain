{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Abstract API for network which support
module Thundermint.P2P.NetworkTls (
    -- * Real tls network
   realNetworkTls
  ) where

import Control.Concurrent.STM

import Control.Concurrent     (forkIO, killThread)
import Control.Monad          (forM_, forever, void, when)
import Control.Monad.Catch    (bracketOnError, onException, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits              (unsafeShiftL)
import Data.List              (find)
import Data.Maybe             (fromMaybe)
import Data.Monoid            ((<>))
import Data.Word              (Word32)
import System.Timeout         (timeout)

import qualified Data.ByteString.Builder        as BB
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map                       as Map
import qualified Network.Socket                 as Net
import qualified Network.Socket.ByteString      as NetBS
import qualified Network.Socket.ByteString.Lazy as NetLBS

import Thundermint.Control
import Thundermint.P2P.Types

----------------------------------------------------------------
--
----------------------------------------------------------------

headerSize :: HeaderSize
headerSize = 4

----------------------------------------------------------------
--
----------------------------------------------------------------
-- | API implementation for tls real tcp network
realNetworkTls :: Net.ServiceName -> NetworkAPI Net.SockAddr
realNetworkTls listenPort = undefined
