{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Thundermint.P2P.Network.Internal.UDP 
  ( newNetworkUdp ) where

import Control.Concurrent.STM

import Data.Word              (Word32, Word8)
import Control.Monad          (forM_, forever, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Bits              (complement)
import Control.Monad.Catch    (onException)
import System.Timeout         (timeout)
import Control.Concurrent     (forkIO, killThread, threadDelay)

import qualified Codec.Serialise           as CBOR
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Map.Strict           as Map
import qualified Network.Socket            as Net
import qualified Network.Socket.ByteString as NetBS

import Thundermint.P2P.Network.RealNetworkStub
import Thundermint.P2P.Types

import qualified Thundermint.P2P.Network.IpAddresses as Ip

-- | API implementation example for real udp network
newNetworkUdp :: PeerInfo -> IO NetworkAPI
newNetworkUdp ourPeerInfo = do
  let serviceName = show $ piPeerPort ourPeerInfo
  -- FIXME: prolly HostName fits better than SockAddr
  tChans <- newTVarIO Map.empty
  acceptChan <- newTChanIO :: IO (TChan (P2PConnection, NetAddr))
  let hints = Net.defaultHints
        { Net.addrFlags      = []
        , Net.addrSocketType = Net.Datagram
        }
  addrInfo':_ <- Net.getAddrInfo (Just hints) Nothing (Just serviceName)
  let changeToWildcard Net.AddrInfo{..} = Net.AddrInfo
        { Net.addrAddress = case addrAddress of
            Net.SockAddrInet6 p f h s
              | h == (0,0,0,1) -> Net.SockAddrInet6 p f (0,0,0,0) s
            _ -> addrAddress
        , ..
        }
      addrInfo = changeToWildcard addrInfo'
  sock <- newUDPSocket addrInfo
  tid  <- forkIO $
    flip onException (Net.close sock) $ do
      Net.bind sock (Net.addrAddress addrInfo)
      forever $ do
        (bs, addr') <- NetBS.recvFrom sock (fromIntegral chunkSize * 2)
        let addr = Ip.normalizeNetAddr $ sockAddrToNetAddr addr'
        case CBOR.deserialiseOrFail $ LBS.fromStrict bs of
          -- silently dropping the packet.
          Left _ -> return ()
          Right (otherPeerInfo, (front, ofs, payload)) -> do
            let connectPacket = isConnectPart (front, ofs, payload)
            atomically $ do
              (found, (recvChan, frontVar, receivedFrontsVar)) <- findOrCreateRecvTuple tChans addr
              when (not found) $ writeTChan acceptChan
                (applyConn otherPeerInfo sock addr frontVar receivedFrontsVar recvChan tChans, addr)
              writeTChan recvChan (otherPeerInfo, (front, ofs, payload))
            when connectPacket $ do
              flip (NetBS.sendAllTo sock) addr' $ LBS.toStrict $ CBOR.serialise (ourPeerInfo, mkAckPart)
  return $ (realNetworkStub ourPeerInfo)
    { listenOn = do
        return (liftIO $ killThread tid, liftIO.atomically $ readTChan acceptChan)
      --
    , connect  = \addr -> liftIO $ do
        (peerChan, connection) <- atomically $ do
          (_, (peerChan, frontVar, receivedFrontsVar)) <- findOrCreateRecvTuple tChans addr
          return ( peerChan
                 , applyConn (PeerInfo (PeerId 0) 0 0) sock addr frontVar receivedFrontsVar peerChan tChans
                 )
        otherPeerInfo <- retryN 20 $ do
          flip (NetBS.sendAllTo sock) (netAddrToSockAddr addr)
            $ LBS.toStrict $ CBOR.serialise (ourPeerInfo, mkConnectPart)
          maybeInfoPayload <- timeout 500000 $ atomically $ readTChan peerChan
          case maybeInfoPayload of
            Nothing -> return Nothing
            Just pkt@(peerInfo, packet) -> do
              let special = isConnectPart packet || isAckPart packet
              when (not special) $ atomically $ writeTChan peerChan pkt
              return $ Just peerInfo
        return $ connection { connectedPeer = otherPeerInfo }
    }
 where
   applyConn otherPeerInfo sock addr frontVar receivedFrontsVar peerChan tChans = P2PConnection
     { send          = liftIO . sendSplitted ourPeerInfo frontVar sock addr
     , recv          = liftIO $ receiveAction receivedFrontsVar peerChan
     , close         = closeConn addr tChans
     , connectedPeer = otherPeerInfo
     }

receiveAction
  :: TVar (Map.Map Word8 [(Word32, LBS.ByteString)])
  -> TChan (PeerInfo, (Word8, Word32, LBS.ByteString))
  -> IO (Maybe LBS.ByteString)
receiveAction frontsVar peerChan = do
  message <- atomically $ do
    (_, (front, ofs, chunk)) <- readTChan peerChan
    fronts <- readTVar frontsVar
    let (newFronts, message) = updateMessages front ofs chunk fronts
    writeTVar frontsVar newFronts
    return message
  if LBS.null message then receiveAction frontsVar peerChan
                      else return $! Just $! LBS.copy message

sendSplitted :: PeerInfo -> TVar Word8 -> Net.Socket -> NetAddr -> LBS.ByteString -> IO ()
sendSplitted ourPeerInfo frontVar sock addr msg = do
  front <- atomically $ do -- slightly overkill, but in line with other's code.
    i <- readTVar frontVar
    writeTVar frontVar $ i + 1
    return (i :: Word8)
  forM_ (zip sleeps splitChunks) $ \(sleep, (ofs, chunk)) -> do
    flip (NetBS.sendAllTo sock) (netAddrToSockAddr addr) $ LBS.toStrict $
      CBOR.serialise (ourPeerInfo, (front :: Word8, ofs :: Word32, chunk))
    when sleep $ threadDelay 100
  where
    splitChunks = splitToChunks msg
    sleeps = cycle (replicate 12 False ++ [True])


mkConnectPart,mkAckPart :: (Word8, Word32, LBS.ByteString)
mkConnectPart = (255, complement 0, LBS.empty)
mkAckPart     = (255, complement 1, LBS.empty)

isConnectPart, isAckPart :: (Word8, Word32, LBS.ByteString) -> Bool
isConnectPart (front, ofs, payload) = front == 255 && ofs == complement 0 && LBS.null payload
isAckPart     (front, ofs, payload) = front == 255 && ofs == complement 1 && LBS.null payload


splitToChunks :: LBS.ByteString -> [(Word32, LBS.ByteString)]
splitToChunks s
  | LBS.length s < 1 = [(0, s)] -- do not lose empty messages.
  | otherwise = go 0 s
  where
    go ofs bs
      | LBS.length bs < 1 = []
      | LBS.length bs == intChunkSize = [(ofs, LBS.copy bs), (ofs + chunkSize, LBS.empty)]
      | otherwise = (ofs, LBS.copy hd) : go (ofs + (fromIntegral $ LBS.length hd)) tl
      where
        intChunkSize = fromIntegral chunkSize
        (hd,tl) = LBS.splitAt intChunkSize bs

findOrCreateRecvTuple :: TVar (Map.Map NetAddr (TChan a1, TVar Word8, TVar (Map.Map k a2)))
                      -> NetAddr
                      -> STM (Bool, (TChan a1, TVar Word8, TVar (Map.Map k a2)))
findOrCreateRecvTuple tChans addr = do
  chans <- readTVar tChans
  case Map.lookup addr chans of
    Just chanFrontVar -> return (True, chanFrontVar)
    Nothing   -> do
      recvChan <- newTChan
      frontVar <- newTVar 0
      receivedFrontsVar <- newTVar Map.empty
      let fullInfo = (recvChan, frontVar, receivedFrontsVar)
      writeTVar tChans $ Map.insert addr fullInfo chans
      return (False, fullInfo)

chunkSize :: Word32
chunkSize = 1400 :: Word32 -- should prevent in-kernel UDP splitting/reassembling most of the time.

newUDPSocket :: Net.AddrInfo -> IO Net.Socket
newUDPSocket ai = do
  sock <- Net.socket (Net.addrFamily     ai)
                     (Net.addrSocketType ai)
                     (Net.addrProtocol   ai)
  Net.setSocketOption sock Net.ReuseAddr 0
  return sock

closeConn :: (MonadIO m, Ord k)
          => k -> TVar (Map.Map k a) -> m ()
closeConn addr tChans = do
  liftIO . atomically $ do
    chans <- readTVar tChans
    writeTVar tChans $ Map.delete addr chans

updateMessages :: Word8 -> Word32 -> LBS.ByteString -> Map.Map Word8 [(Word32, LBS.ByteString)]
               -> (Map.Map Word8 [(Word32, LBS.ByteString)], LBS.ByteString)
updateMessages front ofs !chunk fronts =  (newFronts, extractedMessage)
  where
    listOfPartials = insert (ofs, chunk) $ Map.findWithDefault [] front fronts
    insert ofsChunk [] = [ofsChunk]
    insert ofsChunk@(ofs', _) restPartials@(oc@(headOfs, _):ocs)
      | ofs' < headOfs = ofsChunk : restPartials
      | otherwise = oc : insert ofsChunk ocs
    (invalidPartials, canCombinePartials) = checkPartials False True 0 listOfPartials
    lbsLength = fromIntegral . LBS.length
    checkPartials _ _ _ [] =
      error "internal error: empty list of partials"
    checkPartials invalid canCombine currentPartialLen [(lastOfs, lastChunk)] =
      ( invalid, canCombine && lastOfs == currentPartialLen && lbsLength lastChunk < chunkSize)
    checkPartials invalid canCombine currentPartialLen ((headOfs, headChunk):ocs) =
      checkPartials
        (invalid || lbsLength headChunk /= chunkSize || mod headOfs chunkSize /= 0 || headOfs < currentPartialLen)
        (canCombine && headOfs == currentPartialLen)
        (headOfs + lbsLength headChunk)
        ocs
    (extractedMessage, updatedFront)
      | invalidPartials = (LBS.empty, Map.delete front fronts)
      | canCombinePartials = (LBS.copy $ LBS.concat $ map snd listOfPartials, Map.delete front fronts)
      | otherwise = (LBS.empty, Map.insert front listOfPartials fronts)
    newFronts
      | Map.size updatedFront > 10 = pruneFront updatedFront
      | otherwise = updatedFront

pruneFront :: Map.Map Word8 [(Word32, LBS.ByteString)] -> Map.Map Word8 [(Word32, LBS.ByteString)]
pruneFront fronts
  | maxMinDelta < minMaxDelta = Map.delete minFront fronts
  | otherwise = Map.delete maxFront fronts
  where
    Just ((minFront, _), _) = Map.minViewWithKey fronts
    Just ((maxFront, _), _) = Map.maxViewWithKey fronts
    -- these deltas are modulo 256, which is useful.
    -- thus if we have maxFront 252 and minFront 240, the
    -- maxMinDelta will be 12 and minMaxDelta will be 244.
    -- if minFront is 4 and maxFront is 252 (suggesting that
    -- there was an overflow), the maxMinDelta will be 248
    -- and minMaxDelta will be 8.
    -- shorter delta indicate where oldest front was.
    maxMinDelta = maxFront - minFront
    minMaxDelta = minFront - maxFront

retryN
  :: Int
  -> IO (Maybe a)
  -> IO a
retryN n action = loop n
  where
    loop 0 = error "timeout waiting for 'UDP connection' (actually, peerinfo exchange)."
    loop k = action >>= \case
      Just a  -> return a
      Nothing -> loop (k - 1)
