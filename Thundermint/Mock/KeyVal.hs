{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- |
module Thundermint.Mock.KeyVal (
    genesisBlock
  , transitions
  , executeSpec
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Int
import Data.List

import Data.Map        (Map)
import Data.Maybe      (isJust)
import System.FilePath ((</>))

import qualified Data.Map as Map

import Thundermint.Mock.Store  (newBlockStorage)
import Thundermint.P2P.Network (createMockNode)
import Thundermint.P2P.Network (newMockNet)

import Thundermint.Blockchain.App
import Thundermint.Blockchain.Interpretation
import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.Control
import Thundermint.Crypto            (hash)
import Thundermint.Crypto.Containers (ValidatorSet)
import Thundermint.Crypto.Ed25519
import Thundermint.Logger
import Thundermint.Mock.KeyList
import Thundermint.Mock.Types
import Thundermint.P2P
import Thundermint.Store



----------------------------------------------------------------
--
----------------------------------------------------------------

genesisBlock :: ValidatorSet Ed25519_SHA512 -> Block Ed25519_SHA512 [(String,Int)]
genesisBlock valSet = Block
  { blockHeader = Header
      { headerChainID        = "KV"
      , headerHeight         = Height 0
      , headerTime           = Time 0
      , headerLastBlockID    = Nothing
      , headerValidatorsHash = hash valSet
      }
  , blockData       = []
  , blockLastCommit = Nothing
  }

transitions :: BlockFold (Map String Int) (String,Int) [(String,Int)]
transitions = BlockFold
  { processTx           = const process
  , processBlock        = \_ txs s0 -> foldM (flip process) s0 txs
  , transactionsToBlock = \_ ->
      let selectTx _ []     = []
          selectTx c (t:tx) = case process t c of
                                Nothing -> selectTx c  tx
                                Just c' -> t : selectTx c' tx
      in selectTx
  , initialState        = Map.empty
  }
  where
    process (k,v) m
      | k `Map.member` m = Nothing
      | otherwise        = Just $ Map.insert k v m



-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

interpretSpec
  :: Maybe Int64                -- ^ Maximum height
  -> FilePath
  -> NetSpec                    --
  -> IO [(BlockStorage 'RO IO Ed25519_SHA512 [(String,Int)], IO ())]
interpretSpec maxH prefix NetSpec{..} = do
  net <- newMockNet
  forM (Map.toList netAddresses) $ \(addr, NodeSpec{..}) -> do
    -- Prepare logging
    let loggers = [ makeScribe s { scribe'path = fmap (prefix </>) (scribe'path s) }
                  | s <- nspecLogFile
                  ]
    -- Create storage
    storage  <- newBlockStorage prefix nspecDbName (genesisBlock validatorSet) validatorSet
    hChain   <- blockchainHeight storage
    --
    withLogEnv "TM" "DEV" loggers $ \logenv -> runLoggerT "general" logenv $ do
      -- Blockchain state
      bchState <- newBChState transitions
                $ makeReadOnly (hoistBlockStorageRW liftIO storage)
      _        <- stateAtH bchState (next hChain)
      let appState = AppState
            { appStorage        = hoistBlockStorageRW liftIO storage
            --
            , appValidationFun  = \h a -> do
                st <- stateAtH bchState h
                return $ isJust $ processBlock transitions h a st
            --
            , appBlockGenerator = \h -> case nspecByzantine of
                Just "InvalidBlock" -> do
                  return [("XXX", 123)]
                _ -> do
                  st <- stateAtH bchState h
                  let Just k = find (`Map.notMember` st) ["K_" ++ show (n :: Int) | n <- [1 ..]]
                  return [(k, addr)]
            --
            , appCommitCallback = \case
                h | Just h' <- maxH
                  , h > Height h'   -> throwM Abort
                  | otherwise       -> return ()
            , appValidator        = nspecPrivKey
            , appNextValidatorSet = \_ _ -> return validatorSet
            }
      appCh <- newAppChans
      return ( makeReadOnly storage
             , runLoggerT "general" logenv $ runConcurrently
                 [ setNamespace "net"
                   $ startPeerDispatcher
                       defCfg
                       (createMockNode net "50000" addr)
                       (addr,"50000")
                       (map (,"50000") $ connections netAddresses addr)
                       appCh
                       (hoistBlockStorageRO liftIO $ makeReadOnly storage)
                       nullMempool
                 , setNamespace "consensus"
                   $ runApplication defCfg appState appCh
                 ]
             )
  where
    netAddresses = Map.fromList $ [0::Int ..] `zip` netNodeList
    connections  = case netTopology of
      Ring    -> connectRing
      All2All -> connectAll2All
    validatorSet = makeValidatorSetFromPriv [ pk | Just pk <- nspecPrivKey <$> netNodeList ]


executeSpec
  :: Maybe Int64                -- ^ Maximum height
  -> FilePath
  -> NetSpec                    --
  -> IO [BlockStorage 'RO IO Ed25519_SHA512 [(String,Int)]]
executeSpec maxH prefix spec = do
  actions <- interpretSpec maxH prefix spec
  handleNAbort (length actions) $ runConcurrently (snd <$> actions)
  return $ fst <$> actions
