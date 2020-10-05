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



----------------------------------------------------------------
-- Orphans
----------------------------------------------------------------

instance CryptoAsymmetric a => FromHttpApiData (PublicKey a) where
  parseUrlPiece s = case decodeBase58 s of
    Nothing -> Left "Bad base58 encoding"
    Just k  -> Right k
