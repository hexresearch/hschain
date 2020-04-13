{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
-- |
module HSChain.PoW.P2P.Handler.PEX
  ( runPEX
  ) where

import Codec.Serialise
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Cont
import Control.Monad.IO.Class
import Data.Functor.Contravariant
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
import HSChain.PoW.Exceptions
import HSChain.PoW.Consensus
import HSChain.PoW.Types
import qualified HSChain.Network.IpAddresses as Ip
import HSChain.Types.Merkle.Types

----------------------------------------------------------------
--
----------------------------------------------------------------

runPEX
  :: ( MonadMask m, MonadFork m
     , Serialise (b IdNode)
     , Serialise (b Hashed)
     , BlockData b
     )
  => NetworkAPI
  -> BlockRegistry b
  -> Sink (BoxRX m b)
  -> STM (Src (MsgAnn b))
  -> STM (Consensus m b)
  -> BlockDB m b
  -> ContT r m ()
runPEX netAPI blockReg sinkBOX mkSrcAnn consSt db = do
  reg                <- newPeerRegistry
  nonces             <- newNonceSet
  (sinkAddr,srcAddr) <- queuePair
  (sinkAsk,mkSrcAsk) <- broadcastPair
  catchup            <- newCatchupThrottle
  let mkChans = do
        peerBCastAnn     <- mkSrcAnn
        peerBCastAskPeer <- mkSrcAsk
        return PeerChans
          { sinkNewAddr    = sinkAddr
          , pexGoodPeers   = connectedPeersList reg
          , sinkConsensus  = sinkBOX
          , peerCatchup    = catchup
          , peerReqBlocks  = blockReg
          , peerConsensuSt = consSt
          , peerBlockDB    = db
          , ..
          }
  shepherd <- ContT withShepherd
  cfork $ acceptLoop         netAPI shepherd reg nonces mkChans
  cfork $ monitorConnections netAPI shepherd reg nonces mkChans
  cfork $ monitorKnownPeers reg sinkAsk
  cfork $ processNewAddr    reg srcAddr

acceptLoop
  :: ( MonadMask m, MonadFork m
     , Serialise (b IdNode)
     , Serialise (b Hashed)
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
      -- Expect GossipHello from peer
      HandshakeHello nonce port <- deserialise <$> recv conn
      let normAddr = normalizeNodeAddress addr port
      -- Check nonce for self-connection and send reply
      isSelfConnection nonceSet nonce >>= \case
        True  -> atomicallyIO $ addSelfAddress reg normAddr
        False -> do
          send conn $ serialise HandshakeAck
          withPeer reg normAddr $ runPeer conn =<< atomicallyIO mkChans

connectTo
  :: ( MonadMask m, MonadFork m
     , Serialise (b IdNode)
     , Serialise (b Hashed)
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
    withPeer reg addr $ do
      bracket (connect addr) close $ \conn -> do
        -- Perform handshake
        withHandshakeNonce nonceSet $ \nonce -> do
          send conn $ serialise $ HandshakeHello nonce $ fromIntegral listenPort
          HandshakeAck <- deserialise <$> recv conn
          return ()
        -- Start peer
        runPeer conn chans


-- | Thread that monitor that we have enough connections and tries to acquire more .
monitorKnownPeers
  :: (MonadIO m)
  => PeerRegistry               -- ^ Peer registry
  -> Sink AskPeers              -- ^ Sink for sending AskMorePeer messages
  -> m ()
monitorKnownPeers reg sinkPeers = forever $ do
  -- We block unless we don't have enough connections then we ask
  -- peers for more and wait.
  atomicallyIO $ check . (<10) =<< knownPeers reg
  sinkIO sinkPeers AskPeers
  waitMSec (3000::Int)

monitorConnections
  :: ( MonadMask m, MonadFork m
     , Serialise (b IdNode)
     , Serialise (b Hashed)
     , BlockData b
     )
  => NetworkAPI
  -> Shepherd
  -> PeerRegistry
  -> NonceSet
  -> STM (PeerChans m b)
  -> m ()
monitorConnections netAPI shepherd reg nonceSet mkChans = forever $ do
  -- Check that we need and can connect to peers
  addrs <- atomicallyIO $ do
    nPeers <- connectedPeers reg
    check $ nPeers < expected
    addrs <- availableToConnect reg
    check $ not $ null addrs
    return $ take (expected - nPeers) addrs
  --
  forM_ addrs $ \a -> connectTo netAPI a shepherd reg nonceSet =<< atomicallyIO mkChans
  waitMSec (3000 :: Int)
  where
    expected = 4


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


cfork :: (MonadMask m, MonadFork m) => m a -> ContT b m ()
cfork action = ContT $ \cnt -> forkLinked action (cnt ())
