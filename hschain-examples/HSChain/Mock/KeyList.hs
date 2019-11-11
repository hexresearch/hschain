{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
module HSChain.Mock.KeyList (
    -- * Private keys for testing
    makeValidatorSetFromPriv
  , makePrivKeyStream
    -- * Connection in mock network
  , connectAll2All
  , connectRing
  ) where


import qualified Data.ByteString        as BS
import           Data.Foldable          (toList)
import           Data.List
import           Data.Proxy
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict      as Map
import           System.Random

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Crypto
import HSChain.Types.Validators

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Create set of all known public validators from set of private
--   validators
makeValidatorSetFromPriv
  :: (Foldable f, Crypto alg)
  => f (PrivValidator alg) -> ValidatorSet alg
makeValidatorSetFromPriv vals =
  case r of
    Right x -> x
    Left  e -> error $ "Duplicate public key in validator: " ++ show e
  where
    r = makeValidatorSet
      [ Validator { validatorPubKey      = publicKey (validatorPrivKey v)
                  , validatorVotingPower = 1
                  }
      | v <- toList vals
      ]


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Calculate set of addresses for node to connect to
--   assuming all nodes are connected to each other.
connectAll2All :: Ord addr => Map addr a -> addr -> [addr]
connectAll2All vals addr =
  [ a
  | a <- Map.keys vals
  , a < addr
  ]

-- | Connect nodes in ring topology
connectRing :: Ord addr => Map addr a -> addr -> [addr]
connectRing vals addr =
  case Map.splitLookup addr vals of
    (_ , Nothing, _ ) -> []
    (va, Just _ , vb) -> case Map.lookupMin vb of
      Just (a,_) -> [a]
      Nothing    -> case Map.lookupMin va of
        Just (a,_) -> [a]
        Nothing    -> []


-- | Generate infinite stream of private keys from single seed. This
--   function is very useful for testing since it allows to generate a
--   lot of keys quickly and in deterministic manner. Naturally it
--   shoulldn't be used in production
makePrivKeyStream :: forall alg. CryptoSign alg => Int -> [PrivKey alg]
makePrivKeyStream seed
  = unfoldr step
  $ randoms (mkStdGen seed)
  where
    -- Size of key
    keySize = privKeySize (Proxy @alg)
    -- Generate single key
    step stream = Just (k, stream')
      where
        Just k    = decodeFromBS $ BS.pack bs
        (bs, stream') = splitAt keySize stream
