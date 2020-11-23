{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
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
import HSChain.Control.Util
import HSChain.Crypto
import HSChain.PoW.Consensus
import HSChain.PoW.Mempool
import HSChain.PoW.Types


-- | REST API for interacting with mempool.
data MempoolRestAPI b route = MempoolRestAPI
  { mempoolPostTxAsync :: route :- "post_tx_async"
                                :> ReqBody '[JSON] (Tx b)
                                :> Post '[PlainText] String
    -- | Post TX to mempool without checking whether it's accepted or not.
  , mempoolPostTxSync  :: route :- "post_tx"
                                :> ReqBody '[JSON] (Tx b)
                                :> Post '[JSON] Bool
  , mempoolSize        :: route :- "size" :> Get '[JSON] Int
  }
  deriving Generic

-- | Server implementation
mempoolApiServer
  :: (MonadIO m, b ~ BlockType view)
  => MempoolAPI view -> MempoolRestAPI b (AsServerT m)
mempoolApiServer mempool = MempoolRestAPI
  { mempoolPostTxAsync = postTxEndpoint     mempool
  , mempoolPostTxSync  = postTxSyncEndpoint mempool
  , mempoolSize        = getMempoolSizeEndpoint mempool
  }

postTxEndpoint
  :: (MonadIO m)
  => MempoolAPI view -> TxOf view -> m String
postTxEndpoint MempoolAPI{..} tx = do
  sinkIO postTransaction tx
  return "TRIED"

postTxSyncEndpoint
  :: (MonadIO m)
  => MempoolAPI view -> TxOf view -> m Bool
postTxSyncEndpoint MempoolAPI{..} tx = do
  blockingCall postTransactionSync tx

getMempoolSizeEndpoint :: (MonadIO m) => MempoolAPI view -> m Int
getMempoolSizeEndpoint MempoolAPI{..} = length <$> atomicallyIO mempoolState


----------------------------------------------------------------
-- Orphans
----------------------------------------------------------------

instance CryptoAsymmetric a => FromHttpApiData (PublicKey a) where
  parseUrlPiece s = case decodeBase58 s of
    Nothing -> Left "Bad base58 encoding"
    Just k  -> Right k
