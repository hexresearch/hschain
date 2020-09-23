{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
-- |
module HSChain.PoW.API where

import Control.Monad.IO.Class
import Servant.API
import Servant.API.Generic
import Servant.Server
import Servant.Server.Generic
import GHC.Generics (Generic)

import HSChain.Crypto
import HSChain.Examples.Coin
import HSChain.Store.Query



data CoinAPI route = CoinAPI
  { coinBalance :: route :- "balance" :> Get '[JSON] [(PublicKey Alg, Integer)]
  }
  deriving Generic

coinServer :: (MonadIO m, MonadReadDB m) => CoinAPI (AsServerT m)
coinServer = CoinAPI
  { coinBalance = balanceEndpoint
  }

balanceEndpoint :: (MonadIO m, MonadReadDB m) => m [(PublicKey Alg, Integer)]
balanceEndpoint = do
  rs <- queryRO $ basicQuery_
    "SELECT pk_dest, SUM(n_coins) \
    \  FROM coin_utxo \
    \  JOIN coin_state ON utxo_id = live_utxo \
    \ GROUP BY pk_dest"
  return [(k,i) | (ByteRepred k, i) <- rs]
