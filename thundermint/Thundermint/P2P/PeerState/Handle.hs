{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Thundermint.P2P.PeerState.Handle
 ( module Thundermint.P2P.PeerState.Types
 , handler
 ) where

import Control.Monad.Catch      (MonadThrow)
import Control.Monad.RWS.Strict

import Thundermint.Crypto
import Thundermint.Store.Internal.Query (MonadReadDB)

import Thundermint.P2P.PeerState.Monad
import Thundermint.P2P.PeerState.Types

import qualified Thundermint.P2P.PeerState.Handle.Lagging as Lagging
import qualified Thundermint.P2P.PeerState.Handle.Current as Current
import qualified Thundermint.P2P.PeerState.Handle.Ahead as Ahead
import qualified Thundermint.P2P.PeerState.Handle.Unknown as Unknown


handler :: (CryptoHash alg, CryptoSign alg, Monad m, MonadIO m, MonadReadDB m alg a, MonadThrow m)
       => Config m alg a
       -> SomeState alg a
       -> Event alg a
       -> m (SomeState alg a, [Command alg a])
handler config st event = case st of
  WrapState (Lagging s) -> select <$> runTransitionT (Lagging.handler event) config s
  WrapState (Current s) -> select <$> runTransitionT (Current.handler event) config s
  WrapState (Ahead s)   -> select <$> runTransitionT (Ahead.handler event) config s
  WrapState (Unknown s) -> select <$> runTransitionT (Unknown.handler event) config s
  where
    -- | Drops the middle value from a three-tuple
    select :: (a, b, c) -> (a, c)
    select (a, _, c) = (a, c)

