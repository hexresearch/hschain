{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module HSChain.P2P.PeerState.Handle
 ( module HSChain.P2P.PeerState.Types
 , handler
 ) where

import Control.Monad (foldM)
import Control.Arrow (second)

import HSChain.Crypto (CryptoHashable)
import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Monad
import HSChain.P2P.PeerState.Types
import HSChain.P2P.PeerState.Handle.Utils (handlerGeneric,issuedGossipHandlerGeneric)
import qualified HSChain.P2P.PeerState.Handle.Ahead   as Ahead
import qualified HSChain.P2P.PeerState.Handle.Current as Current
import qualified HSChain.P2P.PeerState.Handle.Lagging as Lagging
import qualified HSChain.P2P.PeerState.Handle.Unknown as Unknown


handler :: (CryptoHashable a, HandlerCtx a m)
        => Config m a
        -> State a
        -> Event a
        -> m (State a, [Command a])
handler config st event = do
  (st',cmds) <- case st of
    Lagging s -> runTransitionT (handlerGeneric Lagging.handler event) config s
    Current s -> runTransitionT (handlerGeneric Current.handler event) config s
    Ahead   s -> runTransitionT (handlerGeneric Ahead.handler   event) config s
    Unknown s -> runTransitionT (handlerGeneric Unknown.handler event) config s
  second (cmds <>) <$> handleIssuedGossip config st' [ c | Push2Gossip c <- cmds ]


handleIssuedGossip :: HandlerCtx a m
                   => Config m a
                   -> State a
                   -> [GossipMsg a]
                   -> m (State a, [Command a])
handleIssuedGossip config st = foldM (\ (s',cmds) msg -> second (cmds<>) <$> case s' of
  Lagging s -> runTransitionT (issuedGossipHandlerGeneric Lagging.issuedGossipHandler msg) config s
  Current s -> runTransitionT (issuedGossipHandlerGeneric Current.issuedGossipHandler msg) config s
  Ahead   s -> runTransitionT (issuedGossipHandlerGeneric Ahead.issuedGossipHandler   msg) config s
  Unknown s -> runTransitionT (issuedGossipHandlerGeneric Unknown.issuedGossipHandler msg) config s ) (st,[])
