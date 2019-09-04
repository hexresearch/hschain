-- |
-- Very simple implementation of one special case of PBKDF2 as use by
-- xenochain.
module HSChain.Crypto.PBKDF2Simple (
    sha512PBKDF2
  ) where

import Data.ByteString (ByteString)
import Crypto.Pbkdf2
import Data.Digest.Pure.SHA (bytestringDigest, hmacSha512)

import qualified Data.ByteString.Lazy as LB

sha512PBKDF2
  :: ByteString                 -- ^ Password (secret)
  -> ByteString                 -- ^ Salt (not necessary secret)
  -> Int                        -- ^ Number of iteration
  -> Int                        -- ^ Desired size of output
  -> ByteString
sha512PBKDF2 password salt iter size =
  LB.toStrict $ LB.take (fromIntegral size) $ pbkdf2 sha512 password salt (fromIntegral iter)
  where
    sha512 key msg = LB.toStrict $ bytestringDigest $ hmacSha512 (LB.fromStrict key) (LB.fromStrict msg)
