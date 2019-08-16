{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Thundermint.P2P.PeerState.Handle
 ( module Thundermint.P2P.PeerState.Types
 , handler
 ) where

import Control.Monad (foldM)
import Control.Arrow (second)
import Data.Maybe    (mapMaybe)

import Thundermint.P2P.Internal.Types
import Thundermint.P2P.PeerState.Monad
import Thundermint.P2P.PeerState.Types

import qualified Thundermint.P2P.PeerState.Handle.Ahead   as Ahead
import qualified Thundermint.P2P.PeerState.Handle.Current as Current
import qualified Thundermint.P2P.PeerState.Handle.Lagging as Lagging
import qualified Thundermint.P2P.PeerState.Handle.Unknown as Unknown


handler :: HandlerCtx alg a m
        => Config m alg a
        -> SomeState alg a
        -> Event alg a
        -> m (SomeState alg a, [Command alg a])
handler config st event = do
  (st',cmds) <- case st of
    WrapState (Lagging s) -> select <$> runTransitionT (Lagging.handler event) config s
    WrapState (Current s) -> select <$> runTransitionT (Current.handler event) config s
    WrapState (Ahead s)   -> select <$> runTransitionT (Ahead.handler event) config s
    WrapState (Unknown s) -> select <$> runTransitionT (Unknown.handler event) config s
  second (cmds <>) <$> do handleIssuedGossip config st' $ mapMaybe getPush2Gossip cmds
    where getPush2Gossip (Push2Gossip c) = Just c
          getPush2Gossip _               = Nothing

--
-- | Drops the middle value from a three-tuple
select :: (a, b, c) -> (a, c)
select (a, _, c) = (a, c)

handleIssuedGossip :: HandlerCtx alg a m
                   => Config m alg a
                   -> SomeState alg a
                   -> [GossipMsg alg a]
                   -> m (SomeState alg a, [Command alg a])
handleIssuedGossip config st = foldM (\ (s',cmds) msg -> second (cmds<>) <$> case s' of
  WrapState (Lagging s) -> select <$> runTransitionT (Lagging.issuedGossipHandler msg) config s
  WrapState (Current s) -> select <$> runTransitionT (Current.issuedGossipHandler msg) config s
  WrapState (Ahead s)   -> select <$> runTransitionT (Ahead.issuedGossipHandler msg) config s
  WrapState (Unknown s) -> select <$> runTransitionT (Unknown.issuedGossipHandler msg) config s ) (st,[])
