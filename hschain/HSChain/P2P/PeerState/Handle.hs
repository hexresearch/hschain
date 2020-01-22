{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
module HSChain.P2P.PeerState.Handle
 ( module HSChain.P2P.PeerState.Types
 , handler
 , handlerTx
 , handlerGossip
 ) where

import Control.Monad (foldM)
import Control.Arrow (second)

import HSChain.Crypto (CryptoHashable)
import HSChain.Blockchain.Internal.Types
import HSChain.P2P.Internal.Types
import HSChain.P2P.PeerState.Monad
import HSChain.P2P.PeerState.Types
import HSChain.P2P.PeerState.Handle.Utils (handlerGeneric,issuedGossipHandlerGeneric,
                                           HandlerDict(..))
import qualified HSChain.P2P.PeerState.Handle.Ahead   as Ahead
import qualified HSChain.P2P.PeerState.Handle.Current as Current
import qualified HSChain.P2P.PeerState.Handle.Lagging as Lagging
import qualified HSChain.P2P.PeerState.Handle.Unknown as Unknown


handler :: (CryptoHashable a, HandlerCtx a m)
        => Config a
        -> State a
        -> GossipTimeout
        -> m (State a, [Command a])
handler = dispatchDictionaries handlerGeneric

handlerTx
  :: (CryptoHashable a, HandlerCtx a m)
  => Config a
  -> State a
  -> MessageTx a
  -> m (State a, [Command a])
handlerTx config st msgTx = do
  second (Push2Gossip msgGsp :) <$> handleIssuedGossip config st [ msgGsp ]
  where
    msgGsp = case msgTx of
      TxAnn       a -> GossipAnn       a
      TxProposal  p -> GossipProposal  p
      TxPreVote   v -> GossipPreVote   v
      TxPreCommit v -> GossipPreCommit v

handlerGossip
  :: (CryptoHashable a, HandlerCtx a m)
  => Config a
  -> State a
  -> GossipMsg a
  -> m (State a, [Command a])
handlerGossip = dispatchDictionaries $ \dct m ->
  resendGossip m >> handlerGossipMsg dct m

handleIssuedGossip :: (CryptoHashable a, HandlerCtx a m)
                   => Config a
                   -> State a
                   -> [GossipMsg a]
                   -> m (State a, [Command a])
handleIssuedGossip config st = foldM (\ (s',cmds) msg -> second (cmds<>) <$> case s' of
  Lagging s -> runTransitionT (issuedGossipHandlerGeneric Lagging.handler msg) config s
  Current s -> runTransitionT (issuedGossipHandlerGeneric Current.handler msg) config s
  Ahead   s -> runTransitionT (issuedGossipHandlerGeneric Ahead.handler   msg) config s
  Unknown s -> runTransitionT (issuedGossipHandlerGeneric Unknown.handler msg) config s ) (st,[])


dispatchDictionaries
  :: ( CryptoHashable a, HandlerCtx a m
     )
  => (forall s. HandlerDict s a m -> msg -> TransitionT s a m ())
  -> Config a
  -> State a
  -> msg
  -> m (State a, [Command a])
dispatchDictionaries fun config st event = do
  (st',cmds) <- case st of
    Lagging s -> runTransitionT (fun Lagging.handler event) config s
    Current s -> runTransitionT (fun Current.handler event) config s
    Ahead   s -> runTransitionT (fun Ahead.handler   event) config s
    Unknown s -> runTransitionT (fun Unknown.handler event) config s
  second (cmds <>) <$> handleIssuedGossip config st' [ c | Push2Gossip c <- cmds ]
