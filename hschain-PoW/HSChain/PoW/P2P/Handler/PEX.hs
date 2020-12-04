-- |
module HSChain.PoW.P2P.Handler.PEX
  ( PexCh(..)
  , runPEX
  ) where

import Codec.Serialise
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Cont
import qualified Data.Text as T
import Katip (sl)
import Data.Word

import HSChain.Control.Class
import HSChain.Control.Channels
import HSChain.Control.Util
import HSChain.Control.Shepherd
import HSChain.Control.Delay
import HSChain.Network.Types
import HSChain.PoW.P2P.STM.NonceSet
import HSChain.PoW.P2P.STM.PeerRegistry
import HSChain.PoW.P2P.Types
import HSChain.PoW.P2P.Handler.Peer
import HSChain.PoW.P2P.Handler.CatchupLock
import HSChain.PoW.P2P.Handler.BlockRequests
import HSChain.PoW.Consensus
import HSChain.PoW.Types
import HSChain.Logger
import qualified HSChain.Network.IpAddresses as Ip
import HSChain.Types.Merkle.Types

----------------------------------------------------------------
--
----------------------------------------------------------------

data PexCh m b = PexCh
  { pexNodeCfg        :: NodeCfg
  , pexNetAPI         :: NetworkAPI
  , pexSinkTX         :: Sink (Tx b)
  , pexSinkBox        :: Sink (BoxRX m b)
  , pexMkAnnounce     :: STM (Src (GossipMsg b))
  , pexConsesusState  :: STM (BlockIndex b, BH b, Locator b)
  }

runPEX
  :: ( MonadMask m, MonadFork m, MonadLogger m
     , Serialise (b Identity)
     , Serialise (b Proxy)
     , Serialise (Tx b)
     , BlockData b
     )
  => PexCh m b
  -> BlockRegistry b
  -> BlockDB m b
  -> ContT r m ()
runPEX PexCh{..} blockReg db = do
  reg                <- newPeerRegistry $ initialPeers pexNodeCfg
  nonces             <- newNonceSet
  (sinkAddr,srcAddr) <- queuePair
  (sinkAsk,mkSrcAsk) <- broadcastPair
  catchup            <- newCatchupThrottle
  let mkChans = do
        peerBCastAskPeer <- mkSrcAsk
        peerBCastAnn     <- pexMkAnnounce
        return PeerChans
          { peerSinkNewAddr    = sinkAddr
          , peerSinkConsensus  = pexSinkBox
          , peerSinkTX         = pexSinkTX
          , peerCatchup        = catchup
          , peerReqBlocks      = blockReg
          , peerConnections    = connectedPeersList reg
          , peerConsensusState = pexConsesusState
          , peerBlockDB        = db
          , ..
          }
  shepherd <- ContT withShepherd
  start "accept"  $ acceptLoop                    pexNetAPI shepherd reg nonces mkChans
  start "conn"    $ monitorConnections pexNodeCfg pexNetAPI shepherd reg nonces mkChans
  start "known"   $ monitorKnownPeers  pexNodeCfg reg sinkAsk
  start "newaddr" $ processNewAddr     reg srcAddr
  where
    start ns = cforkLinked . descendNamespace "net" . descendNamespace ns . logOnException

acceptLoop
  :: ( MonadMask m, MonadFork m, MonadLogger m
     , Serialise (b Identity)
     , Serialise (b Proxy)
     , Serialise (Tx b)
     , BlockData b
     )
  => NetworkAPI
  -> Shepherd
  -> PeerRegistry
  -> NonceSet
  -> STM (PeerChans m b)
  -> m ()
acceptLoop NetworkAPI{..} shepherd reg nonceSet mkChans  = do
  bracket listenOn fst $ \(_,accept) -> forever $ do
    mask $ \restore -> do
      (conn, addr) <- accept
      newSheepFinally shepherd
        (restore $ peerThread conn addr)
        (close conn)
  where
    peerThread conn addr = do
      logger InfoS "Pre-accepted connection" (sl "addr" addr)
      -- Expect GossipHello from peer
      HandshakeHello nonce port <- deserialise <$> recv conn
      let normAddr = normalizeNodeAddress addr port
      -- Check nonce for self-connection and send reply
      isSelfConnection nonceSet nonce >>= \case
        True  -> do
          logger InfoS "Self connection detected" (sl "addr" normAddr)
          atomicallyIO $ addSelfAddress reg normAddr
        False -> do
          send conn $ serialise HandshakeAck
          descendNamespace (T.pack (show normAddr))
            $ withPeer reg normAddr
            $ runPeer conn =<< atomicallyIO mkChans

connectTo
  :: ( MonadMask m, MonadFork m, MonadLogger m
     , Serialise (b Identity)
     , Serialise (b Proxy)
     , Serialise (Tx b)
     , BlockData b
     )
  => NetworkAPI
  -> NetAddr
  -> Shepherd
  -> PeerRegistry
  -> NonceSet
  -> PeerChans m b
  -> m ()
connectTo NetworkAPI{..} addr shepherd reg nonceSet chans =
  newSheep shepherd $ do
    logger InfoS "Connecting to" (sl "addr" addr)
    descendNamespace (T.pack (show addr))
      $ withPeer reg addr
      $ logOnException
      $ bracket (connect addr) close $ \conn -> do
          -- Perform handshake
          logger DebugS "Initial connection established" ()
          withHandshakeNonce nonceSet $ \nonce -> do
            send conn $ serialise $ HandshakeHello nonce $ fromIntegral listenPort
            HandshakeAck <- deserialise <$> recv conn
            return ()
          -- Start peer
          runPeer conn chans


-- | Thread that monitor that we have enough connections and tries to acquire more .
monitorKnownPeers
  :: (MonadIO m)
  => NodeCfg
  -> PeerRegistry               -- ^ Peer registry
  -> Sink AskPeers              -- ^ Sink for sending AskMorePeer messages
  -> m ()
monitorKnownPeers NodeCfg{..} reg sinkPeers = forever $ do
  -- We block unless we don't have enough connections then we ask
  -- peers for more and wait.
  atomicallyIO $ check . (<nKnownPeers) =<< knownPeers reg
  sinkIO sinkPeers AskPeers
  waitMSec (3000::Int)

monitorConnections
  :: ( MonadMask m, MonadFork m, MonadLogger m
     , Serialise (b Identity)
     , Serialise (b Proxy)
     , Serialise (Tx b)
     , BlockData b
     )
  => NodeCfg
  -> NetworkAPI
  -> Shepherd
  -> PeerRegistry
  -> NonceSet
  -> STM (PeerChans m b)
  -> m ()
monitorConnections NodeCfg{..} netAPI shepherd reg nonceSet mkChans = forever $ do
  -- Check that we need and can connect to peers
  addrs <- atomicallyIO $ do
    nPeers <- connectedPeers reg
    check $ nPeers < nConnectedPeers
    addrs <- availableToConnect reg
    check $ not $ null addrs
    return $ take (nConnectedPeers - nPeers) addrs
  --
  forM_ addrs $ \a -> connectTo netAPI a shepherd reg nonceSet =<< atomicallyIO mkChans
  waitMSec (3000 :: Int)


processNewAddr
  :: (MonadIO m)
  => PeerRegistry
  -> Src [NetAddr]
  -> m ()
processNewAddr reg srcAddr = forever $ do
  atomicallyIO $ addKnownAddresses reg =<< await srcAddr

normalizeNodeAddress :: NetAddr -> Word16 -> NetAddr
normalizeNodeAddress = flip setPort . Ip.normalizeNetAddr
  where
    setPort port (NetAddrV4 ha _) = NetAddrV4 ha $ fromIntegral port
    setPort port (NetAddrV6 ha _) = NetAddrV6 ha $ fromIntegral port
