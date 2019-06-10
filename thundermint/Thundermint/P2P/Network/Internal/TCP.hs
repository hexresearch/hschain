module Thundermint.P2P.Network.Internal.TCP where

import qualified Codec.Serialise as CBOR

import Control.Monad          (when)
import Control.Monad.Catch    (bracketOnError, throwM)
import Control.Monad.IO.Class (liftIO)
import Data.List              (find)
import Data.Monoid            ((<>))
import Data.Word              (Word32)
import System.Timeout         (timeout)

import qualified Data.ByteString.Builder        as BB
import qualified Data.ByteString.Lazy           as LBS
import qualified Network.Socket                 as Net
import qualified Network.Socket.ByteString.Lazy as NetLBS

import Thundermint.Control
import Thundermint.P2P.Network.Internal.Utils
import Thundermint.P2P.Network.RealNetworkStub
import Thundermint.P2P.Types


-- | API implementation for real tcp network
newNetworkTcp :: PeerInfo -> NetworkAPI
newNetworkTcp peerInfo = (realNetworkStub peerInfo)
  { listenOn = do
      let hints = Net.defaultHints
            { Net.addrFlags      = [Net.AI_PASSIVE]
            , Net.addrSocketType = Net.Stream
            }
      addrs <- liftIO $ Net.getAddrInfo (Just hints) Nothing (Just serviceName)
      addr  <- case () of
        _ | Just a <- find isIPv6addr addrs -> return a
          | a:_    <- addrs                    -> return a
          | otherwise                          -> throwM NoAddressAvailable
      --
      bracketOnError (liftIO $ newSocket addr) (liftIO . Net.close) $ \sock -> liftIO $ do
        when (isIPv6addr addr) $
          Net.setSocketOption sock Net.IPv6Only 0
        Net.bind sock (Net.addrAddress addr)
        Net.listen sock 5
        return (liftIO $ Net.close sock, accept sock)
  --
  , connect  = \addr -> do
      let hints = Just Net.defaultHints
            { Net.addrSocketType = Net.Stream
            }
      (hostName, serviceName') <- liftIO $ Net.getNameInfo
                                            [Net.NI_NUMERICHOST, Net.NI_NUMERICSERV]
                                            True
                                            True
                                            $ netAddrToSockAddr addr
      addrInfo:_ <- liftIO $ Net.getAddrInfo hints hostName serviceName'
      bracketOnError (newSocket addrInfo) (liftIO . Net.close) $ \ sock -> do
        let tenSec = 10000000
        -- Waits for connection for 10 sec and throws `ConnectionTimedOut` exception
        liftIO $ throwNothingM ConnectionTimedOut
               $ timeout tenSec
               $ Net.connect sock $ netAddrToSockAddr addr
        liftIO $ sendBS sock $ CBOR.serialise peerInfo
        mbOtherPeerInfo <- liftIO $ recvBS sock
        case fmap CBOR.deserialiseOrFail mbOtherPeerInfo of
          Nothing -> fail $ "connection dropped while receiving peer info from " ++ show addr
          Just (Left  _      ) -> fail $ "failure to decode PeerInfo from " ++ show addr
          Just (Right otherPI) -> return $ applyConn sock otherPI
  }
 where
  serviceName = show $ piPeerPort peerInfo
  accept sock = do
    (conn, addr) <- liftIO $ Net.accept sock
    liftIO $ sendBS conn $ CBOR.serialise peerInfo
    mbOtherPeerInfo <- liftIO $ recvBS conn
    case fmap CBOR.deserialiseOrFail mbOtherPeerInfo of
      Nothing -> fail $ "connection dropped while receiving peer info from " ++ show addr
      Just (Left  _      ) -> fail $ "failure to decode PeerInfo from " ++ show addr
      Just (Right otherPI) -> return (applyConn conn otherPI, sockAddrToNetAddr addr)
  applyConn conn = P2PConnection (liftIO . sendBS conn) (liftIO $ recvBS conn) (liftIO $ Net.close conn)
  sendBS sock =  \s -> NetLBS.sendAll sock (BB.toLazyByteString $ toFrame s)
                 where
                   toFrame msg = let len =  fromIntegral (LBS.length msg) :: Word32
                                     hexLen = BB.word32BE len
                                 in (hexLen <> BB.lazyByteString msg)
  recvBS sock = do
    header <- recvAll sock headerSize
    if LBS.null header
    then return Nothing
    else let len = decodeWord16BE header
         in case len of
              Just n  -> Just <$> recvAll sock (fromIntegral n)
              Nothing -> return Nothing


-- | helper function read given length of bytes
recvAll :: Net.Socket -> Int -> IO LBS.ByteString
recvAll sock n = LBS.concat `fmap` loop (fromIntegral n)
  where
    loop 0    = return []
    loop left = do
      r <- NetLBS.recv sock left
      if LBS.null r
      then return []
      else fmap (r:) (loop (left - LBS.length r))

