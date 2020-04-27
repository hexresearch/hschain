{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Data types for network
module HSChain.PoW.P2P.Types
  ( -- * Handshake messages
    HandshakeNonce(..)
  , HandshakeHello(..)
  , HandshakeAck(..)
    -- * Wire messages
  , GossipMsg(..)
  , MsgTX
  , MsgRequest(..)
  , MsgResponce(..)
  , MsgAnn(..)
    -- * Internal messages
    -- ** Consensus engine
  , MsgRX(..)
  , CmdPeer(..)
  , BoxRX(..)
  , SentRequest(..)
  , AskPeers(..)
    -- ** 
    -- * Channel dictionaries
  , PeerChans(..)

  , CatchupThrottle(..)
  , ReleaseCatchupThrottle(..)
  , NetCfg(..)
  ) where

import Control.Concurrent.STM
import Control.Monad.State.Strict (StateT)
import Codec.Serialise            (Serialise)
import Data.Word
import qualified Data.Aeson as JSON
import GHC.Generics               (Generic)

import HSChain.Network.Types
import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.Types.Merkle.Types
import HSChain.Control.Channels
import HSChain.PoW.P2P.Handler.BlockRequests



data NetCfg = NetCfg
  { nKnownPeers     :: !Int
  , nConnectedPeers :: !Int
  }

----------------------------------------------------------------
-- Handshake
----------------------------------------------------------------

-- | Random nonce which is used to detect self-connections
newtype HandshakeNonce = HandshakeNonce Word64
  deriving newtype (Show,Eq,Ord,Serialise,JSON.ToJSON)

-- | Message sent by node initiating connection
data HandshakeHello = HandshakeHello !HandshakeNonce !Word16
  deriving stock    (Show,Eq,Generic)
  deriving anyclass (Serialise)

-- | Message sent as reply to HandshakeHello
data HandshakeAck = HandshakeAck
  deriving stock    (Show,Eq,Generic)
  deriving anyclass (Serialise)


----------------------------------------------------------------
-- Wire messages
----------------------------------------------------------------

-- | Messages that are exchanged on the wire
data GossipMsg b
  = -- GossipTX   !(MsgTX b)
    GossipReq  !(MsgRequest b)
  | GossipResp !(MsgResponce b)
  | GossipAnn  !(MsgAnn b)
  deriving stock (Generic)
deriving stock instance (Show (BlockID b), Show (b Proxy), Show (b Identity)) => Show (GossipMsg b)

instance ( Serialise (b Identity)
         , Serialise (b Proxy)
         , Serialise (BlockID b)
         ) => Serialise (GossipMsg b)

  
-- | Messages used for transmitting unconfirmed transactions
data MsgTX b
  deriving stock    (Generic)
  deriving anyclass (Serialise)

-- | Messages used for exchanging information about blockchain: blocks, headers
data MsgRequest b
  = ReqHeaders !(Locator b)
  -- ^ Request headers from peer. It should reply by sending headers
  --   from the longest chain starting from top block in @Locator@
  --   value. If longest chain does not descent from it it should pick
  --   latest common ancestor.
  | ReqBlock   !(BlockID b)
  -- ^ Request block with given BID
  | ReqPeers
  -- ^ Request addresses of known good peers
  deriving stock (Generic)
deriving stock instance Show (BlockID b) => Show (MsgRequest b)

instance ( Serialise (b Proxy)
         , Serialise (BlockID b)
         ) => Serialise (MsgRequest b)

  
data MsgResponce b
  = RespHeaders  ![Header b]
  -- ^ Respond to 'ReqHeaders'  
  | RespBlock    !(Block b)
  -- ^ Respond to 'ReqBlock'
  | RespPeers    ![NetAddr]
  -- ^ Respond to 'ReqPeers'
  | RespNack
  -- ^ Responce was denied.
  deriving stock (Generic)
deriving stock instance (Show (BlockID b), Show (b Proxy), Show (b Identity)) => Show (MsgResponce b)

instance ( Serialise (b Identity)
         , Serialise (b Proxy)
         , Serialise (BlockID b)
         ) => Serialise (MsgResponce b)


data MsgAnn b
  = AnnBestHead !(Header b)
  -- ^ Send new best head to peer. Generally sent unannounced.
  deriving stock (Generic)
deriving stock instance (Show (BlockID b), Show (b Proxy)) => Show (MsgAnn b)


instance ( Serialise (b Proxy)
         , Serialise (BlockID b)
         ) => Serialise (MsgAnn b)

instance ( JSON.ToJSON (BlockID b)
         , forall g. IsMerkle g => JSON.ToJSON (b g)
         ) => JSON.ToJSON (MsgAnn b)



----------------------------------------------------------------
-- Messages for consensus engine
----------------------------------------------------------------

-- | Message to consensus engine
data MsgRX b
  = RxAnn     !(MsgAnn b)  -- ^ Announcement from peer
  | RxBlock   !(Block b)   -- ^ Peer sent block to us
  | RxHeaders [Header b]   -- ^ Peer sent headers to us

-- | Box wrapping message to consensus. 
newtype BoxRX m b = BoxRX
  (forall s. (MsgRX b -> StateT s m (CmdPeer b)) -> StateT s m ())


----------------------------------------------------------------
-- Messages for peer
----------------------------------------------------------------

-- | Command sent to a peer
data CmdPeer b
  = Peer'Punish
  | Peer'EnterCatchup
  | Peer'Noop
  deriving (Show,Eq)

data AskPeers = AskPeers

-- | Channels for peer for communication with rest of the world
data PeerChans m b = PeerChans
  { peerSinkNewAddr   :: Sink [NetAddr]      --
  , peerSinkConsensus :: Sink (BoxRX m b)    --
  , peerBCastAnn      :: Src  (MsgAnn b)
  , peerBCastAskPeer  :: Src   AskPeers
  , peerCatchup       :: CatchupThrottle
  , peerReqBlocks     :: BlockRegistry b
  , peerConnections   :: STM [NetAddr]      --
  , peerConsensuSt    :: STM (Consensus m b)    
  , peerBlockDB       :: BlockDB m b  
  }

data SentRequest b
  = SentBlock   !(BlockID b)
  | SentHeaders ReleaseCatchupThrottle
  | SentPeers
  

----------------------------------------------------------------
-- Global channels
----------------------------------------------------------------


-- | Global catchup lock which is used to throtte catchup attempts. We
--   don't want to send requests for headers to all peers. This locks
--   ensures that we send only requests to a single peer at a time. On
--   the other hand lock must time out since peer can die while
--   request is in flight or just being relally slow.
newtype CatchupThrottle = CatchupThrottle
  { acquireCatchup :: STM ReleaseCatchupThrottle
  }

-- | Release lock.
newtype ReleaseCatchupThrottle = ReleaseCatchupThrottle
  { releaseCatchupLock :: STM ()
  }
