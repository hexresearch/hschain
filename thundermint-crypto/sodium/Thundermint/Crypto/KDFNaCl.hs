{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}
-- |
module Thundermint.Crypto.KDFNaCl (KDFNaCl) where

import Thundermint.Crypto


data KDFNaCl

instance ByteReprSized (KDFOutput KDFNaCl) where
  type ByteSize (KDFOutput KDFNaCl) = 32

instance CryptoKDF KDFNaCl where
  type KDFParams KDFNaCl = ()
  deriveKey () bs = undefined
