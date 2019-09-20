{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module HSChain.P2P.PeerState.Handle
 ( module HSChain.P2P.PeerState.Types
 , handler
 ) where

import Control.Monad (foldM)
import Control.Arrow (second)
import Data.Maybe    (mapMaybe)

import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Monad
import HSChain.P2P.PeerState.Types

import qualified HSChain.P2P.PeerState.Handle.Ahead   as Ahead
import qualified HSChain.P2P.PeerState.Handle.Current as Current
import qualified HSChain.P2P.PeerState.Handle.Lagging as Lagging
import qualified HSChain.P2P.PeerState.Handle.Unknown as Unknown


handler :: HandlerCtx alg a m
        => Config m alg a
        -> State alg a
        -> Event alg a
        -> m (State alg a, [Command alg a])
handler config st event = do
  (st',cmds) <- case st of
    Lagging s -> runTransitionT (Lagging.handler event) config s
    Current s -> runTransitionT (Current.handler event) config s
    Ahead   s -> runTransitionT (Ahead.handler event) config s
    Unknown s -> runTransitionT (Unknown.handler event) config s
  second (cmds <>) <$> do handleIssuedGossip config st' $ mapMaybe getPush2Gossip cmds
    where getPush2Gossip (Push2Gossip c) = Just c
          getPush2Gossip _               = Nothing

handleIssuedGossip :: HandlerCtx alg a m
                   => Config m alg a
                   -> State alg a
                   -> [GossipMsg alg a]
                   -> m (State alg a, [Command alg a])
handleIssuedGossip config st = foldM (\ (s',cmds) msg -> second (cmds<>) <$> case s' of
  Lagging s -> runTransitionT (Lagging.issuedGossipHandler msg) config s
  Current s -> runTransitionT (Current.issuedGossipHandler msg) config s
  Ahead   s -> runTransitionT (Ahead.issuedGossipHandler msg) config s
  Unknown s -> runTransitionT (Unknown.issuedGossipHandler msg) config s ) (st,[])
