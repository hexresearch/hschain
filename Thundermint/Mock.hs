{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Helper function for running mock network of thundermint nodes
module Thundermint.Mock (
    -- * Validators
    makePrivateValidators
  , makeValidatorSetFromPriv
    -- * Network connectivity
  , connectAll2All
  , connectRing
    -- * Running nodes
  , startNode
  , runNodeSet
  ) where

import Codec.Serialise          (Serialise)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Map                 (Map)
import Data.Word                (Word64)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString.Char8  as BC8
import qualified Data.Map               as Map
import qualified Katip
import System.Random (randomIO)
import Text.Printf

import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Crypto.Ed25519   (Ed25519_SHA512, privateKey)
import Thundermint.Blockchain.App
import Thundermint.Blockchain.Types
import Thundermint.Logger
import Thundermint.P2P
import Thundermint.P2P.Network
import Thundermint.Store

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Generate list of private validators
makePrivateValidators
  :: [BS.ByteString]
  -> Map (Address Ed25519_SHA512) (PrivValidator Ed25519_SHA512)
makePrivateValidators keys = Map.fromList
  [ (address (publicKey pk) , PrivValidator pk)
  | bs <- keys
  , let pk = case Base58.decodeBase58 Base58.bitcoinAlphabet bs of
          Just x  -> privateKey x
          Nothing -> error "Incorrect Base58 encoding for bs"
  ]

-- | Create set of all known public validators from set of private
--   validators
makeValidatorSetFromPriv
  :: (Foldable f, Crypto alg)
  => f (PrivValidator alg) -> ValidatorSet alg
makeValidatorSetFromPriv vals =
  case r of
    Right x -> x
    Left  e -> error $ "Dublicate public key in validator: " ++ show e
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


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Start node which will now run consensus algorithm
startNode
  :: (Ord addr, Show addr, Crypto alg, Serialise a, Show a)
  => NetworkAPI sock addr
  -> [addr]
  -> AppState IO alg a
  -> IO ()
startNode net addrs appState@AppState{..} = do
  -- Initialize logging
  logfile <- case appValidator of
    Just (PrivValidator pk) ->
      let Address nm = address $ publicKey pk
      in return $ "val-" ++ BC8.unpack (Base58.encodeBase58 Base58.bitcoinAlphabet nm)
    Nothing -> do
      w1 <- randomIO
      w2 <- randomIO
      return $ printf "node-%016x-%016x" (w1 :: Word64) (w2 :: Word64)
  scribe <- Katip.mkFileScribe ("logs/" ++ logfile) Katip.DebugS Katip.V2
  logenv <- Katip.registerScribe "log" scribe Katip.defaultScribeSettings
        =<< Katip.initLogEnv "TM" "DEV"
  flip finally (Katip.closeScribes logenv) $ do
    appCh   <- newAppChans
    let netRoutine = runLoggerT "net" logenv
                   $ startPeerDispatcher net addrs appCh
                       (makeReadOnly   appStorage)
                       (makeReadOnlyPS appPropStorage)
    withAsync netRoutine $ \_ ->
      runLoggerT "consensus" logenv
        $ runApplication (hoistAppState liftIO appState) appCh

-- | Start set of nodes and return their corresponding storage. Will
--   return their storage after all nodes finish execution
runNodeSet
  :: (Ord addr, Show addr, Crypto alg, Serialise a, Show a)
  => [( NetworkAPI sock addr, [addr], AppState IO alg a)]
  -> IO [BlockStorage 'RO IO alg a]
runNodeSet nodes = do
  withAsyncs [ startNode net addrs appSt
             | (net,addrs,appSt) <- nodes
             ]
    $ void . waitAny
  return [ makeReadOnly $ appStorage a | (_,_,a) <- nodes ]

withAsyncs :: [IO a] -> ([Async a] -> IO b) -> IO b
withAsyncs ios function
  = recur ([],ios)
  where
    recur (as,[])   = function (reverse as)
    recur (as,i:is) = withAsync i $ \a -> recur (a:as, is)
