{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
--
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
module HSChain.PoW.API where

import Control.Monad.IO.Class
import Servant.API
import Servant.API.Generic
import Servant.Server.Generic
-- import Web.HttpApiData

import HSChain.Crypto
import HSChain.Examples.Coin
import HSChain.Store.Query



data CoinAPI route = CoinAPI
  { coinBalance  :: route :- "balance"   :> Get '[JSON] [(PublicKey Alg, Integer)]
  , coinListUTXO :: route :- "list_utxo" :> Capture "pk" (PublicKey Alg) :> Get '[JSON] [UTXO]
  }
  deriving Generic

coinServer :: (MonadIO m, MonadReadDB m) => CoinAPI (AsServerT m)
coinServer = CoinAPI
  { coinBalance  = balanceEndpoint
  , coinListUTXO = listUtxoEndpoint
  }


balanceEndpoint :: (MonadIO m, MonadReadDB m) => m [(PublicKey Alg, Integer)]
balanceEndpoint = do
  rs <- queryRO $ basicQuery_
    "SELECT pk_dest, SUM(n_coins) \
    \  FROM coin_utxo \
    \  JOIN coin_state ON utxo_id = live_utxo \
    \ GROUP BY pk_dest"
  return [(k,i) | (ByteRepred k, i) <- rs]

listUtxoEndpoint :: (MonadIO m, MonadReadDB m) => PublicKey Alg -> m [UTXO]
listUtxoEndpoint pk = do
  queryRO $ basicQuery
    "SELECT n_out, tx_hash \
    \  FROM coin_utxo \
    \  JOIN coin_state ON utxo_id = live_utxo \
    \ WHERE pk_dest = ?"
    (Only (ByteRepred pk))

----------------------------------------------------------------
-- Orphans
----------------------------------------------------------------

instance CryptoAsymmetric a => FromHttpApiData (PublicKey a) where
  parseUrlPiece s = case decodeBase58 s of
    Nothing -> Left "Bad base58 encoding"
    Just k  -> Right k
