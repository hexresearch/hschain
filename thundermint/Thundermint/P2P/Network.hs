{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Abstract API for network which support
module Thundermint.P2P.Network (
    NetworkAPI(..)
  , P2PConnection(..)
    -- * Real network
  , realNetwork
  , realNetworkUdp
    -- * Real tls network
  , realNetworkTls
  , getCredential
  , getCredentialFromBuffer
    -- * Mock in-memory network
  , MockNet
  , newMockNet
  , createMockNode
    -- * Local address detection
  , Ip.getLocalAddress
  , Ip.isLocalAddress
  , Ip.getLocalAddresses
  , PeerInfo(..)
  ) where

import qualified Codec.Serialise as CBOR

import Control.Concurrent.STM

import Control.Concurrent     (forkIO, killThread, threadDelay)
import Control.Monad          (forM_, forever, void, when)
import Control.Monad.Catch    (bracketOnError, onException, throwM)
import Control.Monad.IO.Class (liftIO)
import Data.Bits              (unsafeShiftL, complement)
import Data.List              (find)
import Data.Monoid            ((<>))
import Data.Word              (Word32, Word8)
import System.Timeout         (timeout)

import qualified Data.ByteString.Builder        as BB
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import qualified Network.Socket                 as Net
import qualified Network.Socket.ByteString      as NetBS
import qualified Network.Socket.ByteString.Lazy as NetLBS

import Thundermint.Control
import Thundermint.P2P.Network.TLS
import Thundermint.P2P.Network.RealNetworkStub
import Thundermint.P2P.Types
import qualified Thundermint.P2P.Network.IpAddresses as Ip

-- | API implementation for real tcp network
realNetwork :: PeerInfo -> NetworkAPI
realNetwork ourPeerInfo = (realNetworkStub ourPeerInfo)
  { listenOn = do
      let hints = Net.defaultHints
            { Net.addrFlags      = [Net.AI_PASSIVE]
            , Net.addrSocketType = Net.Stream
            }
      addrs <- liftIO $ Net.getAddrInfo (Just hints) Nothing (Just serviceName)
      addr  <- case () of
        _ | Just a <- find Ip.isIPv6addr addrs -> return a
          | a:_    <- addrs                    -> return a
          | otherwise                          -> throwM NoAddressAvailable
      --
      bracketOnError (liftIO $ newSocket addr) (liftIO . Net.close) $ \sock -> liftIO $ do
        when (Ip.isIPv6addr addr) $
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
        liftIO $ sendBS sock $ CBOR.serialise ourPeerInfo
        mbOtherPeerInfo <- liftIO $ recvBS sock
        case fmap CBOR.deserialiseOrFail mbOtherPeerInfo of
          Nothing -> fail $ "connection dropped while receiving peer info from " ++ show addr
          Just (Left  _      ) -> fail $ "failure to decode PeerInfo from " ++ show addr
          Just (Right otherPI) -> return $ applyConn sock otherPI
  }
 where
  serviceName = show $ piPeerPort ourPeerInfo
  accept sock = do
    (conn, addr) <- liftIO $ Net.accept sock
    liftIO $ sendBS conn $ CBOR.serialise ourPeerInfo
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


decodeWord16BE :: LBS.ByteString -> Maybe Word32
decodeWord16BE bs | LBS.length bs < fromIntegral headerSize = Nothing
                  | otherwise =
                      let w8s = LBS.unpack $ LBS.take (fromIntegral headerSize) bs
                          shiftBy = (*) 8
                          word32 = foldr (\b (acc, i) ->
                                         (fromIntegral b `unsafeShiftL` shiftBy i + acc, i + 1))
                                   (0,0)
                                   w8s
                      in (Just $ fst word32)

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

-- | API implementation example for real udp network
realNetworkUdp :: PeerInfo -> IO NetworkAPI
realNetworkUdp ourPeerInfo = do
  let serviceName = show $ piPeerPort ourPeerInfo
  -- FIXME: prolly HostName fits better than SockAddr
  tChans <- newTVarIO Map.empty
  acceptChan <- newTChanIO :: IO (TChan (P2PConnection, NetAddr))
  let hints = Net.defaultHints
        { Net.addrFlags      = []
        , Net.addrSocketType = Net.Datagram
        }
  addrInfo':_ <- Net.getAddrInfo (Just hints) Nothing (Just serviceName)
  let changeToWildcard ai@(Net.AddrInfo{..})
        = ai
            { Net.addrAddress = case addrAddress of
              Net.SockAddrInet6 p f h s
                | h == (0,0,0,1) -> Net.SockAddrInet6 p f (0,0,0,0) s
              _ -> addrAddress
            }
      addrInfo = changeToWildcard addrInfo'
  sock       <- newUDPSocket addrInfo
  tid <- forkIO $
    flip onException (Net.close sock) $ do
      Net.bind sock (Net.addrAddress addrInfo)
      forever $ do
        (bs, addr') <- NetBS.recvFrom sock (fromIntegral chunkSize * 2)
        let addr = sockAddrToNetAddr $ Ip.normalizeIpAddr addr'
            lazyByteString = LBS.fromStrict bs
            peerInfoPayloadTupleDecoded = CBOR.deserialiseOrFail lazyByteString
        case peerInfoPayloadTupleDecoded of
          Left _ -> -- silently dropping the packet.
            return ()
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
         (peerChan, connection) <- atomically $ (\(_, (peerChan, frontVar, receivedFrontsVar)) ->
               (peerChan, applyConn (PeerInfo (PeerId 0) 0 0) sock addr frontVar receivedFrontsVar peerChan tChans))
           <$> findOrCreateRecvTuple tChans addr
         let waitLoop 0 _ _ = fail "timeout waiting for 'UDP connection' (actually, peerinfo exchange)."
             waitLoop n partialConnection@P2PConnection{..} receiveChan = do
               flip (NetBS.sendAllTo sock) (netAddrToSockAddr addr) $ LBS.toStrict $ CBOR.serialise (ourPeerInfo, mkConnectPart)
               maybeInfoPayload <- timeout 500000 $ atomically $ readTChan receiveChan
               case maybeInfoPayload of
                 Nothing -> waitLoop (n-1 :: Int) partialConnection receiveChan
                 Just pkt@(peerInfo, packet) -> do
                   let special = isConnectPart packet || isAckPart packet
                   when (not special) $ atomically $ writeTChan receiveChan pkt
                   return peerInfo
         otherPeerInfo <- waitLoop 20 connection peerChan
         return $ connection { connectedPeer = otherPeerInfo }
    }
 where
  mkConnectPart = (255 :: Word8, complement 0 :: Word32, LBS.empty)
  mkAckPart = (255 :: Word8, complement 1 :: Word32, LBS.empty)
  isConnectPart (front, ofs, payload) = front == 255 && ofs == complement 0 && LBS.length payload == 0
  isAckPart (front, ofs, payload) = front == 255 && ofs == complement 1 && LBS.length payload == 0
  newUDPSocket ai = do
    sock <- Net.socket (Net.addrFamily     ai)
                       (Net.addrSocketType ai)
                       (Net.addrProtocol   ai)
    Net.setSocketOption sock Net.ReuseAddr 1
    return sock
  findOrCreateRecvTuple tChans addr = do
    chans <- readTVar tChans
    case Map.lookup (addr :: NetAddr) chans of
      Just chanFrontVar -> return (True, chanFrontVar)
      Nothing   -> do
        recvChan <- newTChan
        frontVar <- newTVar (0 :: Word8)
        receivedFrontsVar <- newTVar Map.empty
        let fullInfo = (recvChan, frontVar, receivedFrontsVar)
        writeTVar tChans $ Map.insert addr fullInfo chans
        return (False, fullInfo)
  applyConn otherPeerInfo sock addr frontVar receivedFrontsVar peerChan tChans = P2PConnection
    (\s -> liftIO.void $ sendSplitted frontVar sock addr s)
    (liftIO $ receiveAction addr receivedFrontsVar peerChan)
    (closeConn addr tChans)
    otherPeerInfo
  receiveAction addr frontsVar peerChan = do
    (message) <- atomically $ do
      (_peerInfo, (front, ofs, chunk)) <- readTChan peerChan
      fronts <- readTVar frontsVar
      let (newFronts, message) = updateMessages front ofs chunk fronts
      writeTVar frontsVar newFronts
      return (message)
    if LBS.null message then receiveAction addr frontsVar peerChan else let c = LBS.copy message in c `seq` return $ Just c
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
  sendSplitted frontVar sock addr msg = do
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
  chunkSize = 1400 :: Word32 -- should prevent in-kernel UDP splitting/reassembling most of the time.
  sleeps = cycle (replicate 12 False ++ [True])
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
  closeConn addr tChans = do
    liftIO . atomically $ do
      chans <- readTVar tChans
      writeTVar tChans $ Map.delete addr chans


----------------------------------------------------------------
-- Some useful utilities
----------------------------------------------------------------

-- | Sockets for mock network
data MockSocket = MockSocket
  { msckActive :: !(TVar Bool)
  , msckSend   :: !(TChan LBS.ByteString)
  , msckRecv   :: !(TChan LBS.ByteString)
  }
  deriving (Eq)

-- | Mock network which uses STM to deliver messages
newtype MockNet = MockNet
  { mnetIncoming :: TVar (Map.Map NetAddr [(MockSocket, NetAddr)])
    -- ^ Incoming connections for node.
  }


newMockNet :: IO (MockNet)
newMockNet = MockNet <$> newTVarIO Map.empty


closeMockSocket :: MockSocket -> STM ()
closeMockSocket MockSocket{..} = writeTVar msckActive False


createMockNode
  :: MockNet
  -> NetAddr
  -> NetworkAPI
createMockNode MockNet{..} addr = NetworkAPI
  { listenOn = liftIO.atomically $ do
      let key = addr
      -- Start listening on port
      do mListen <- readTVar mnetIncoming
         case key `Map.lookup` mListen of
           Just  _ -> error "MockNet: already listening on port"
           Nothing -> writeTVar mnetIncoming $ Map.insert key [] mListen
      -- Stop listening and close all accepted sockets
      let stopListening = liftIO.atomically $ do
            mListen <- readTVar mnetIncoming
            forM_ (key `Map.lookup` mListen) $
              mapM_ (closeMockSocket . fst)
      -- Accept connection
      let accept = liftIO.atomically $ do
            mList <- readTVar mnetIncoming
            case key `Map.lookup` mList of
              Nothing     -> error "MockNet: cannot accept on closed socket"
              Just []     -> retry
              Just ((conn,addr'):xs) -> do
                writeTVar mnetIncoming $ Map.insert key xs mList
                return (applyConn addr' conn, addr')
      return (stopListening, accept)
    --
  , connect = \loc -> do
    liftIO.atomically $ do
      chA <- newTChan
      chB <- newTChan
      v   <- newTVar True
      let sockTo   = MockSocket { msckActive = v
                                , msckRecv   = chA
                                , msckSend   = chB
                                }
      let sockFrom = MockSocket { msckActive = v
                                , msckRecv   = chB
                                , msckSend   = chA
                                }
      -- Queue connection on server
      cmap <- readTVar mnetIncoming
      case loc `Map.lookup` cmap of
        Nothing -> error "MockNet: Cannot connect to closed socket"
        Just xs -> writeTVar mnetIncoming $ Map.insert loc (xs ++ [(sockFrom,addr)]) cmap
      return $ applyConn loc sockTo
  , filterOutOwnAddresses = return . Set.filter ((addr /=))
  , normalizeNodeAddress = const
  , listenPort = 0
  , ourPeerInfo = mkPeerInfoFromAddr addr
  }
 where
  mkPeerInfoFromAddr (NetAddrV4 ha _) = PeerInfo (PeerId (fromIntegral ha)) 0 0
  mkPeerInfoFromAddr _ = error "IPv6 addr in mkPeerInfoFromAddr"
  applyConn otherAddr conn = P2PConnection (liftIO . sendBS conn) (liftIO $ recvBS conn) (liftIO $ close conn) (mkPeerInfoFromAddr otherAddr)
  sendBS MockSocket{..} bs = atomically $
      readTVar msckActive >>= \case
        False -> error "MockNet: Cannot write to closed socket"
        True  -> writeTChan msckSend bs
    --
  recvBS MockSocket{..} = atomically $
      readTVar msckActive >>= \case
        False -> tryReadTChan msckRecv
        True  -> Just <$> readTChan msckRecv
    --
  close = atomically . closeMockSocket
