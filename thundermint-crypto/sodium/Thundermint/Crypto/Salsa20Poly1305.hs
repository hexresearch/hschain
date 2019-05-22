{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
module Thundermint.Crypto.Salsa20Poly1305 (Salsa20Poly1305) where

import Control.Monad
import Control.Monad.IO.Class
import Control.DeepSeq (NFData(..))
import Data.Data       (Data)
import qualified Data.ByteString as BS
import Thundermint.Crypto

data Salsa20Poly1305
  deriving (Data)

newtype instance CypherKey Salsa20Poly1305 = Key ()
  deriving newtype (Eq, Ord, NFData)

newtype instance CypherNonce Salsa20Poly1305 = Nonce ()
  deriving newtype (Eq, Ord, NFData)

instance ByteReprSized (CypherKey Salsa20Poly1305) where
  type ByteSize (CypherKey Salsa20Poly1305) = 32 
instance ByteReprSized (CypherNonce Salsa20Poly1305) where
  type ByteSize (CypherNonce Salsa20Poly1305) = 24


instance StreamCypher Salsa20Poly1305 where
  encryptMessage (Key key) (Nonce nonce) msg
    =  undefined
  --
  decryptMessage (Key key) (Nonce nonce) secretMsg = undefined
  --
  generateCypherKey = undefined
  generateCypherNonce = undefined

    

instance ByteRepr (CypherKey Salsa20Poly1305) where
  decodeFromBS bs
    | BS.length bs == 32 = Just $! undefined
    | otherwise          = Nothing
  encodeToBS (Key k) = undefined

instance ByteRepr (CypherNonce Salsa20Poly1305) where
  decodeFromBS bs
    | BS.length bs == 24 = Just $! undefined
    | otherwise          = Nothing
  encodeToBS (Nonce k) = undefined

