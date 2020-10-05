{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
--
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
module HSChain.PoW.API
  ( MempoolRestAPI(..)
  , mempoolApiServer
  ) where

import Control.Monad.IO.Class
import Servant.API
import Servant.API.Generic
import Servant.Server.Generic

import HSChain.Control.Channels
import HSChain.Crypto
import HSChain.PoW.Mempool
import HSChain.PoW.Types


-- | REST API for interacting with mempool.
data MempoolRestAPI b route = MempoolRestAPI
  { mempoolPostTx :: route :- "post_tx"
                           :> ReqBody '[JSON] (Tx b)
                           :> Post '[PlainText] String

  }
  deriving Generic

-- | Server implementation
mempoolApiServer :: (MonadIO m) => MempoolAPI m b -> MempoolRestAPI b (AsServerT m)
mempoolApiServer mempool = MempoolRestAPI
  { mempoolPostTx = postTxEndpoint mempool
  }

postTxEndpoint
  :: (MonadIO m)
  => MempoolAPI m b -> Tx b -> m String
postTxEndpoint MempoolAPI{..} tx = do
  sinkIO postTransaction tx
  return "TRIED"


----------------------------------------------------------------
-- Orphans
----------------------------------------------------------------

instance CryptoAsymmetric a => FromHttpApiData (PublicKey a) where
  parseUrlPiece s = case decodeBase58 s of
    Nothing -> Left "Bad base58 encoding"
    Just k  -> Right k
