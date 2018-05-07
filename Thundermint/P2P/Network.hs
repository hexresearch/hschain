-- |
-- Abstract API for network which support 
module Thundermint.P2P.Network (
    NetworkAPI(..)
  , realNetwork
  ) where

import Control.Exception
import qualified Data.ByteString            as BS
import qualified Network.Socket             as Net
import qualified Network.Socket.ByteString  as NetBS

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Dictionary with API for network programming. We use it to be able
--   to provide two implementations of networking. One is real network
--   and another is mock in-process network for testing.
data NetworkAPI sock addr = NetworkAPI
  { listenOn :: Net.ServiceName -> IO (sock, IO (sock, addr))
    -- ^ Start listening on given port. Returns socket and function
    --   for accepting new connections
  , connect  :: addr -> IO sock
    -- ^ Connect to remote address
  , sendBS   :: sock -> BS.ByteString -> IO ()
    -- ^ Send data to socket
  , recvBS   :: sock -> Int -> IO (Maybe (BS.ByteString))
    -- ^ Receive data from socket
  }


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | API implementation for real network
realNetwork :: NetworkAPI Net.Socket Net.SockAddr
realNetwork = NetworkAPI
  { listenOn = \port -> do
      let hints = Net.defaultHints
            { Net.addrFlags      = [Net.AI_PASSIVE]
            , Net.addrSocketType = Net.Stream
            }
      addr:_ <- Net.getAddrInfo (Just hints) Nothing (Just port)
      sock   <- Net.socket (Net.addrFamily     addr)
                           (Net.addrSocketType addr)
                           (Net.addrProtocol   addr)
      flip onException (Net.close sock) $ do
        Net.bind sock (Net.addrAddress addr)
        return (sock, Net.accept sock)
    --
  , connect  = \addr -> do      
      -- FIXME: we need to support both IP4 & IP6
      sock <- case addr of
        Net.SockAddrInet port _ -> Net.socket Net.AF_INET Net.Stream (fromIntegral port)
        _                       -> error "Unsupported address"
      flip onException (Net.close sock) $ do
        Net.connect sock addr
        return sock
    --
  , sendBS = NetBS.sendAll
  , recvBS = \sock n -> do
      bs <- NetBS.recv sock n
      return $ if BS.null bs then Nothing else Just bs
  }
