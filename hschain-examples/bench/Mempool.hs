{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
-- |
module Mempool (benchmarks) where


import Control.Monad
import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Concurrent.STM

import Criterion.Main

import HSChain.Store
import HSChain.Store.STM
import HSChain.Crypto.Ed25519 (Ed25519)
import HSChain.Crypto.SHA     (SHA512)
import HSChain.Crypto         ((:&))


type BenchMempool m = Mempool m (Ed25519 :& SHA512) Int

benchmarks :: Benchmark
benchmarks =
    env (createBenchMempool 1000000) $ \ ~mempool ->
        bgroup "Mempool" $
            [ bench "test" $ nfAppIO mempool1 mempool
            ]


mempool1 :: BenchMempool IO -> IO ()
mempool1 mempool = do
    filterMempool mempool
    -- txs <- mempoolSize mempool
    -- when (txs > 100000) $ error "UPS"
    -- error ("XXXXX: " ++ show txs)
    -- return ()

createBenchMempool :: Int -> IO (BenchMempool IO)
createBenchMempool size = do
    let checkTx i = return (odd i)
    mempool <- newMempool checkTx
    cursor  <- getMempoolCursor mempool
    mapM_ (pushTransaction cursor) [1..size]
    return mempool

instance NFData (BenchMempool IO) where
    rnf _ = ()

instance NFData (IO ()) where
    rnf _ = ()

