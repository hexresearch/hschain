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
module HSChain.PoW.API where

import Control.Monad.IO.Class
import Servant.API
import Servant.API.Generic
import Servant.Server.Generic
-- import Web.HttpApiData
import Database.SQLite.Simple            ((:.)(..))

import HSChain.Control.Channels
import HSChain.Crypto
import HSChain.Examples.Coin
import HSChain.Store.Query
import HSChain.PoW.Mempool


data CoinAPI route = CoinAPI
  { coinBalance  :: route :- "balance"   :> Get '[JSON] [(PublicKey Alg, Integer)]
  , coinListUTXO :: route :- "list_utxo" :> Capture "pk" (PublicKey Alg) :> Get '[JSON] [UTXO]
  , coinMakeTx   :: route :- "make_tx"
                          :> Capture "from" (PublicKey Alg)
                          :> Capture "N"    Integer
                          :> Capture "to"   (PublicKey Alg)
                          :> Get '[JSON] (Maybe TxSend)
  , coinPostTx   :: route :- "post_tx"
                          :> ReqBody '[JSON] TxCoin
                          :> Post '[PlainText] String
  }
  deriving Generic

coinServer :: (MonadIO m, MonadReadDB m) => MempoolAPI m Coin -> CoinAPI (AsServerT m)
coinServer mempool = CoinAPI
  { coinBalance  = balanceEndpoint
  , coinListUTXO = listUtxoEndpoint
  , coinMakeTx   = makeTxEndpoint
  , coinPostTx   = postTxEndpoint mempool
  }

postTxEndpoint
  :: (MonadIO m, MonadReadDB m)
  => MempoolAPI m Coin -> TxCoin -> m String
postTxEndpoint MempoolAPI{..} tx = do
  sinkIO postTransaction tx
  return "TRIED"
  

makeTxEndpoint
  :: (MonadIO m, MonadReadDB m)
  => PublicKey Alg -> Integer -> PublicKey Alg -> m (Maybe TxSend)
makeTxEndpoint pkFrom toSend pkTo = do
  -- Select all UTXOs in random order
  utxos <- queryRO $ basicQuery
    "SELECT n_out, tx_hash, pk_dest, n_coins \
    \  FROM coin_utxo  \
    \  JOIN coin_state ON utxo_id = live_utxo \
    \ WHERE pk_dest = ?"
    (Only (ByteRepred pkFrom))
  return $ do
    (tot,ins) <- selectUTXO 0 utxos
    return TxSend
      { txInputs  = ins
      , txOutputs = Unspent pkTo toSend
                  : if | tot > toSend -> [Unspent pkFrom (tot - toSend)]
                       | otherwise    -> []
      }
  where
    selectUTXO acc _
      | acc >= toSend = Just (acc,[])
    selectUTXO _  []  = Nothing
    selectUTXO acc ((u :. Unspent _ n):rest) = do
      (tot,us) <- selectUTXO (acc + n) rest
      return (tot, u:us)

  
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
