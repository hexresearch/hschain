{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module HSChain.P2P.Network.Internal.UDP 
  ( newNetworkUdp ) where

import Control.Concurrent.STM

import Data.Word              (Word32, Word8)
import Control.Monad          (forM_, forever, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Word
import Control.Concurrent     (forkFinally, killThread, threadDelay)

import qualified Codec.Serialise           as CBOR
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Map.Strict           as Map
import qualified Network.Socket            as Net
import qualified Network.Socket.ByteString as NetBS

import HSChain.P2P.Types

import qualified HSChain.P2P.Network.IpAddresses as Ip

-- | API implementation example for real udp network
newNetworkUdp :: Word16 -> IO NetworkAPI
newNetworkUdp ourPeerInfo = do
  -- FIXME: prolly HostName fits better than SockAddr
  tChans      <- newTVarIO Map.empty
  acceptChan  <- newTChanIO :: IO (TChan (P2PConnection, NetAddr))
  addrInfo':_ <- Net.getAddrInfo (Just udpHints) Nothing
    (Just $ show ourPeerInfo)
  let changeToWildcard Net.AddrInfo{..} = Net.AddrInfo
        { Net.addrAddress = case addrAddress of
            Net.SockAddrInet6 p f h s
              | h == (0,0,0,1) -> Net.SockAddrInet6 p f (0,0,0,0) s
            _ -> addrAddress
        , ..
        }
      addrInfo = changeToWildcard addrInfo'
  sock <- newUDPSocket addrInfo
  tid  <- flip forkFinally (\_ -> Net.close sock) $ do
      Net.bind sock (Net.addrAddress addrInfo)
      forever $ do
        (bs, addr') <- NetBS.recvFrom sock (fromIntegral chunkSize * 2)
        let addr = Ip.normalizeNetAddr $ sockAddrToNetAddr addr'
        case CBOR.deserialiseOrFail $ LBS.fromStrict bs of
          -- silently dropping the packet.
          Left _ -> return ()
          Right (otherPeerInfo, (front, ofs, payload)) -> do
            atomically $ do
              (found, (recvChan, frontVar, receivedFrontsVar)) <- findOrCreateRecvTuple tChans addr
              when (not found) $ writeTChan acceptChan
                (applyConn ourPeerInfo sock addr frontVar receivedFrontsVar recvChan tChans, addr)
              writeTChan recvChan (otherPeerInfo, (front, ofs, payload))
  return NetworkAPI
    { listenOn = do
        return ( liftIO $ killThread tid
               , liftIO $ atomically $ readTChan acceptChan
               )
      --
    , connect  = \addr -> liftIO $ do
        atomically $ do
          (_, (peerChan, frontVar, receivedFrontsVar)) <- findOrCreateRecvTuple tChans addr
          return $ applyConn ourPeerInfo
                     sock addr frontVar receivedFrontsVar peerChan tChans
      --
    , listenPort = fromIntegral ourPeerInfo
    }
 where


applyConn
  :: Word16
  -> Net.Socket
  -> NetAddr
  -> TVar  Word8
  -> TVar  (Map.Map Word8 [(Word32, LBS.ByteString)])
  -> TChan (Word16, (Word8, Word32, LBS.ByteString))
  -> TVar  (Map.Map NetAddr a)
  -> P2PConnection
applyConn ourPeerInfo sock addr frontVar receivedFrontsVar peerChan tChans = P2PConnection
  { send          = liftIO . sendSplitted ourPeerInfo frontVar sock addr
  , recv          = liftIO $ receiveAction receivedFrontsVar peerChan
  , close         = closeConn addr tChans
  }

receiveAction
  :: TVar (Map.Map Word8 [(Word32, LBS.ByteString)])
  -> TChan (Word16, (Word8, Word32, LBS.ByteString))
  -> IO LBS.ByteString
receiveAction frontsVar peerChan = do
  message <- atomically $ do
    (_, (front, ofs, chunk)) <- readTChan peerChan
    fronts <- readTVar frontsVar
    let (newFronts, message) = updateMessages front ofs chunk fronts
    writeTVar frontsVar newFronts
    return message
  if LBS.null message then receiveAction frontsVar peerChan
                      else return $! LBS.copy message

sendSplitted :: Word16 -> TVar Word8 -> Net.Socket -> NetAddr -> LBS.ByteString -> IO ()
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

-- should prevent in-kernel UDP splitting/reassembling most of the time.
chunkSize :: Word32
chunkSize = 1400

newUDPSocket :: Net.AddrInfo -> IO Net.Socket
newUDPSocket ai = do
  sock <- Net.socket (Net.addrFamily     ai)
                     (Net.addrSocketType ai)
                     (Net.addrProtocol   ai)
  Net.setSocketOption sock Net.ReuseAddr 0
  return sock

closeConn :: (MonadIO m, Ord k)
          => k -> TVar (Map.Map k a) -> m ()
closeConn addr tChans = liftIO $ atomically $ do
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
