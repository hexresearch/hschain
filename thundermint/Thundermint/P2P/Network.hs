{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Abstract API for network which support
module Thundermint.P2P.Network (
    NetworkAPI(..)
  , P2PConnection(..)
    -- * Real network stub
  , realNetworkStub
    -- * Real network
  , realNetwork
  , realNetworkUdp
    -- * Real tls network
  , realNetworkTls
  , getCredential
  , getCredentialFromBuffer
    -- * Mock in-memory network
  , MockSocket
  , MockNet
  , newMockNet
  , createMockNode
    -- * Local address detection
  , Ip.getLocalAddress
  , Ip.isLocalAddress
  , Ip.getLocalAddresses
  ) where

import qualified Codec.Serialise as CBOR

import Control.Concurrent.STM

import Control.Concurrent     (forkIO, killThread, threadDelay)
import Control.Monad          (forM_, forever, void, when)
import Control.Monad.Catch    (bracketOnError, onException, throwM)
import Control.Monad.IO.Class (liftIO)
import Data.Bits              (unsafeShiftL)
import Data.List              (find)
import Data.Maybe             (fromMaybe)
import Data.Monoid            ((<>))
import Data.Word              (Word32, Word8)
import System.Timeout         (timeout)

import qualified Data.ByteString.Builder        as BB
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import qualified Network.Socket                 as Net
import qualified Network.Socket.ByteString      as NetBS
import qualified Network.Socket.ByteString.Lazy as NetLBS

import Thundermint.Control
import Thundermint.P2P.Network.TLS
import Thundermint.P2P.Network.RealNetworkStub
import Thundermint.P2P.Types
import qualified Thundermint.P2P.Network.IpAddresses as Ip

import Debug.Trace

-- | API implementation for real tcp network
realNetwork :: PeerInfo -> Net.ServiceName -> NetworkAPI
realNetwork ourPeerInfo serviceName = (realNetworkStub serviceName)
  { listenOn = do
      let hints = Net.defaultHints
            { Net.addrFlags      = [Net.AI_PASSIVE]
            , Net.addrSocketType = Net.Stream
            }
      addrs <- liftIO $ Net.getAddrInfo (Just hints) Nothing (Just serviceName)

      when (null addrs) $
        throwM NoAddressAvailable
      let addr = fromMaybe (head addrs) $ find isIPv6addr addrs

      bracketOnError (liftIO $ newSocket addr) (liftIO . Net.close) $ \sock -> liftIO $ do
        when (isIPv6addr addr) $
          Net.setSocketOption sock Net.IPv6Only 0
        Net.bind sock (Net.addrAddress addr)
        Net.listen sock 5
        return (liftIO $ Net.close sock, accept sock)
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
          Just (Left err) -> fail $ "failure to decode PeerInfo from " ++ show addr
          Just (Right otherPI) -> return $ applyConn otherPI sock
  }
 where
  accept sock = do
    (conn, addr) <- liftIO $ Net.accept sock
    liftIO $ sendBS conn $ CBOR.serialise ourPeerInfo
    mbOtherPeerInfo <- liftIO $ recvBS sock
    case fmap CBOR.deserialiseOrFail mbOtherPeerInfo of
      Nothing -> fail $ "connection dropped while receiving peer info from " ++ show addr
      Just (Left err) -> fail $ "failure to decode PeerInfo from " ++ show addr
      Just (Right otherPI) -> return (applyConn otherPI sock, sockAddrToNetAddr addr)
  applyConn pi conn = P2PConnection (liftIO . sendBS conn) (liftIO $ recvBS conn) (liftIO $ Net.close conn) pi
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

-- |Shared code - we need checking of IPv6 in several different places.
isIPv6addr :: Net.AddrInfo -> Bool
isIPv6addr = (==) Net.AF_INET6 . Net.addrFamily

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


emptyBs2Maybe :: LBS.ByteString -> Maybe LBS.ByteString
emptyBs2Maybe bs
  | LBS.null bs = Nothing
  | otherwise  = Just bs

-- | API implementation example for real udp network
realNetworkUdp :: Net.ServiceName -> IO NetworkAPI
realNetworkUdp serviceName = do
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
        atomically $ do
          (recvChan, frontVar, receivedFrontsVar) <- findOrCreateRecvTuple tChans addr
          writeTChan acceptChan
            (applyConn sock addr frontVar receivedFrontsVar recvChan tChans, addr)
          writeTChan recvChan $ LBS.fromStrict bs

  return $ NetworkAPI
    { listenOn = do
        return (liftIO $ killThread tid, liftIO.atomically $ readTChan acceptChan)
      --
    , connect  = \addr ->
         liftIO . atomically $ (\(peerChan, frontVar, receivedFrontsVar) ->
               applyConn sock addr frontVar receivedFrontsVar peerChan tChans)
           <$> findOrCreateRecvTuple tChans addr
    , filterOutOwnAddresses = filterOutOwnAddresses (realNetworkStub serviceName)
    , normalizeNodeAddress = normalizeNodeAddress (realNetworkStub serviceName)
    , listenPort = listenPort (realNetworkStub serviceName)
    }
 where
  newUDPSocket ai = do
    sock <- Net.socket (Net.addrFamily     ai)
                       (Net.addrSocketType ai)
                       (Net.addrProtocol   ai)
    Net.setSocketOption sock Net.ReuseAddr 1
    return sock
  findOrCreateRecvTuple tChans addr = do
    chans <- readTVar tChans
    case Map.lookup (addr :: NetAddr) chans of
      Just chanFrontVar -> return chanFrontVar
      Nothing   -> do
        recvChan <- newTChan
        frontVar <- newTVar (0 :: Word8)
        receivedFrontsVar <- newTVar Map.empty
        writeTVar tChans $ Map.insert addr (recvChan, frontVar, receivedFrontsVar) chans
        return (recvChan, frontVar, receivedFrontsVar)
  applyConn sock addr frontVar receivedFrontsVar peerChan tChans = P2PConnection
    (\s -> liftIO.void $ sendSplitted frontVar sock addr s)
    (liftIO $ receiveAction receivedFrontsVar peerChan)
    (close addr tChans)
  receiveAction frontsVar peerChan = do
    (message, logMsg) <- atomically $ do
      serializedTriple <- readTChan peerChan
      case CBOR.deserialiseOrFail serializedTriple of
        Right (front, ofs, chunk) -> do
          fronts <- readTVar frontsVar
          let (newFronts, message) = updateMessages front ofs chunk fronts
          writeTVar frontsVar newFronts
          return (message, "")
        Left err -> return (LBS.empty, "unable to deserialize packet: " ++ show err)
    if null logMsg
      then return $ emptyBs2Maybe message
      else do
        putStrLn $ "error in assembling packet: " ++ logMsg
        return Nothing
  updateMessages :: Word8 -> Word32 -> LBS.ByteString -> Map.Map Word8 [(Word32, LBS.ByteString)]
                 -> (Map.Map Word8 [(Word32, LBS.ByteString)], LBS.ByteString)
  updateMessages front ofs chunk fronts = traceEvent ("ZZZZ: list of partials' lengths: "++show (map (\(o,c) -> (o,lbsLength c)) listOfPartials))
    traceEvent ("ZZZZ: canCombine, invalid: "++show (canCombinePartials, invalidPartials)) (newFronts, extractedMessage)
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
        traceEvent ("ZZZZ: at headOfs "++show headOfs++" invalid is "++show invalid++" and canCombined is "++show canCombine) $
        traceEvent ("ZZZZ: at headOfs "++show headOfs++" current partial len is "++show currentPartialLen) $
        checkPartials
          (invalid || lbsLength headChunk /= chunkSize || mod headOfs chunkSize /= 0 || headOfs < currentPartialLen)
          (canCombine && headOfs == currentPartialLen)
          (headOfs + lbsLength headChunk)
          ocs
      (extractedMessage, updatedFront)
        | invalidPartials = (LBS.empty, Map.delete front fronts)
        | canCombinePartials = (LBS.concat $ map snd listOfPartials, Map.delete front fronts)
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
      flip (NetBS.sendAllTo sock) (netAddrToSockAddr addr) $ LBS.toStrict $ CBOR.serialise (front :: Word8, ofs :: Word32, chunk)
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
  close addr tChans = do
    liftIO . atomically $ do
      chans <- readTVar tChans
      writeTVar tChans $ Map.delete addr chans


----------------------------------------------------------------
-- Some useful utilities
----------------------------------------------------------------

--showSockAddr :: Net.SockAddr -> String
--showSockAddr s@(Net.SockAddrInet pn ha) =
--    unwords ["SockAddrInet", show pn, show ha, "(" <> show s <> ")"]
--showSockAddr s@(Net.SockAddrInet6 pn fi ha si) =
--    unwords ["SockAddrInet6 ", show pn, show fi, show ha, show si, "(" <> show s <> ")"]
--showSockAddr s = "?? (" <> show s <> ")"


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
                return (applyConn conn, addr')
      return (stopListening, accept)
    --
  , connect = \loc -> liftIO.atomically $ do
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
        Just xs -> writeTVar mnetIncoming $ Map.insert loc (xs ++ [(sockFrom,loc)]) cmap
      return $ applyConn sockTo
  , filterOutOwnAddresses = return . Set.filter ((addr /=))
  , normalizeNodeAddress = const
  , listenPort = 0
  }
 where
  applyConn conn = P2PConnection (liftIO . sendBS conn) (liftIO $ recvBS conn) (liftIO $ close conn)
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
