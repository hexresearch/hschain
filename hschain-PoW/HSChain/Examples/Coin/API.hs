{-# LANGUAGE DataKinds #-}
-- |
module HSChain.Examples.Coin.API where

import Control.Monad.IO.Class
import Servant.API
import Servant.API.Generic
import Servant.Server.Generic

import HSChain.Crypto
import HSChain.Examples.Coin
import HSChain.PoW.API
import HSChain.PoW.Mempool
import HSChain.Store.Query


data CoinAPI route = CoinAPI
  { coinBalance  :: route :- "balance"   :> Get '[JSON] [(PublicKey Alg, Integer)]
  , coinListUTXO :: route :- "list_utxo" :> Capture "pk" (PublicKey Alg) :> Get '[JSON] [UTXO]
  , coinMakeTx   :: route :- "make_tx"
                          :> Capture "from" (PublicKey Alg)
                          :> Capture "N"    Integer
                          :> Capture "to"   (PublicKey Alg)
                          :> Get '[JSON] (Maybe TxSend)
  , coinMempool  :: route :- "mempool"
                          :> ToServantApi (MempoolRestAPI Coin)
  }
  deriving Generic

coinServer :: (MonadIO m, MonadReadDB m) => MempoolAPI (CoinState m) -> CoinAPI (AsServerT m)
coinServer mempool = CoinAPI
  { coinBalance  = balanceEndpoint
  , coinListUTXO = listUtxoEndpoint
  , coinMakeTx   = makeTxEndpoint
  , coinMempool  = toServant (mempoolApiServer mempool)
  }

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
