{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Salsa20 stream cypher with Poly1305 MAC. It uses same format as tweet NaCl.
module Thundermint.Crypto.Salsa20Poly1305 where

import Control.Monad
import Control.Monad.IO.Class
import Control.DeepSeq (NFData(..))
import Data.Data       (Data)
import Data.ByteArray
import System.Entropy  (getEntropy)
import qualified Data.ByteString      as BS
import qualified Crypto.Cipher.XSalsa as XSalsa
import qualified Crypto.MAC.Poly1305  as Poly1305

import Thundermint.Crypto


data Salsa20Poly1305
  deriving (Data)

newtype instance CypherKey Salsa20Poly1305 = Key ScrubbedBytes
  deriving newtype (Eq, Ord, NFData)

newtype instance CypherNonce Salsa20Poly1305 = Nonce ScrubbedBytes
  deriving newtype (Eq, Ord, NFData)

instance StreamCypher Salsa20Poly1305 where
  type instance CypherKeySize   Salsa20Poly1305 = 32
  type instance CypherNonceSize Salsa20Poly1305 = 24
  --
  encryptMessage (Key key) (Nonce nonce) msg
    =  convert tag
    <> ciphertext
    where
      -- We generate 32-byte key for Poly1305 from Salsa key stream
      st                           = XSalsa.initialize 20 key nonce
      (macKey::ScrubbedBytes, st') = XSalsa.generate st  32
      (ciphertext           , _  ) = XSalsa.combine  st' msg
      --
      tag = Poly1305.auth macKey ciphertext
  --
  decryptMessage (Key key) (Nonce nonce) secretMsg = do
    -- Split MAC and ciphertext
    let (mac, ciphertext) = BS.splitAt 16 secretMsg
    guard $ BS.length mac == 16
    -- Check that MAC is correct
    let st                           = XSalsa.initialize 20 key nonce
        (macKey::ScrubbedBytes, st') = XSalsa.generate st 32
        computedMac :: ScrubbedBytes
        computedMac = convert $ Poly1305.auth macKey ciphertext
    guard $ convert mac == computedMac
    -- Decrypt message
    return $ fst $ XSalsa.combine st' ciphertext
  --
  generateCypherKey = do
    bs <- liftIO $ getEntropy 32
    return $! Key $ convert bs
  --
  generateCypherNonce = do
    bs <- liftIO $ getEntropy 24
    return $! Nonce $ convert bs

    

instance ByteRepr (CypherKey Salsa20Poly1305) where
  decodeFromBS bs
    | BS.length bs == 32 = Just $! Key $ convert bs
    | otherwise          = Nothing
  encodeToBS (Key k) = convert k

instance ByteRepr (CypherNonce Salsa20Poly1305) where
  decodeFromBS bs
    | BS.length bs == 24 = Just $! Nonce $ convert bs
    | otherwise          = Nothing
  encodeToBS (Nonce k) = convert k
