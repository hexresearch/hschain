{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Thundermint.Crypto.Swear where

import Thundermint.Crypto

import Crypto.Hash     (Digest, MD5, hash)
import Data.ByteArray  (convert)
import Data.ByteString (ByteString, pack)
import Data.Word       (Word8)

md5 :: ByteString -> ByteString
md5 = convert . asMD5 . hash
 where
  asMD5 :: Digest MD5 -> Digest MD5
  asMD5 = id

-- Mock crypto which works as long as no one tries to break it.
data Swear

newtype instance PrivKey   Swear = SwearPrivK Word8
newtype instance PublicKey Swear = SwearPubK Word8
-- newtype instance Signature Swear = SwearSig ()

-- | We assume that there's
instance Crypto Swear where
  signBlob            _ _   = Signature ""
  verifyBlobSignature _ _ _ = True
  publicKey (SwearPrivK w)  = SwearPubK w
  address   (SwearPubK  w)  = Address $ pack [w]
  hashBlob                  = Hash . md5

