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
import Data.Int
import Data.List
import Data.Typeable   (Proxy(..))

import Data.Map        (Map)
import System.FilePath ((</>))

import qualified Data.Map as Map

import Thundermint.P2P.Network (createMockNode)
import Thundermint.P2P.Network (newMockNet)

import Thundermint.Blockchain.Internal.Engine
import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Blockchain.Interpretation
import Thundermint.Types.Blockchain
import Thundermint.Control
import Thundermint.Crypto.Ed25519
import Thundermint.Logger
import Thundermint.Mock.KeyList
import Thundermint.Mock.Types
import Thundermint.P2P
import Thundermint.Run
import Thundermint.Store
import Thundermint.Store.Internal.Query (Connection,connectionRO)
import Thundermint.Types.Validators (ValidatorSet)


----------------------------------------------------------------
--
----------------------------------------------------------------

genesisBlock :: ValidatorSet Ed25519_SHA512 -> Block Ed25519_SHA512 [(String,NetAddr)]
genesisBlock valSet
  = makeGenesis "KV" (Time 0) [] valSet

transitions :: BlockFold (Map String NetAddr) alg [(String,NetAddr)]
transitions = BlockFold
  { processTx           = const $ const process
  , processBlock        = \_ b s0 -> foldM (flip process) s0 (blockData b)
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
  -> NetSpec NodeSpec
  -> IO [(Connection 'RO Ed25519_SHA512 [(String,NetAddr)], IO ())]
interpretSpec maxH prefix NetSpec{..} = do
  net <- newMockNet
  forM (Map.toList netAddresses) $ \(addr, NodeSpec{..}) -> do
    -- Prepare logging
    let loggers = [ makeScribe s { scribe'path = fmap (prefix </>) (scribe'path s) }
                  | s <- nspecLogFile
                  ]
    -- Create storage
    conn <- openConnection (maybe ":memory:" (prefix </>) nspecDbName)
    initDatabase conn Proxy (genesisBlock validatorSet) validatorSet
    runDBT conn $ do
      hChain <- queryRO blockchainHeight
      return ( connectionRO conn
             , runDBT conn $ withLogEnv "TM" "DEV" loggers $ \logenv -> runLoggerT logenv $ do
                 -- Blockchain state
                 bchState <- newBChState transitions
                 _        <- stateAtH bchState (succ hChain)
                 let appState = AppLogic
                       { appValidationFun  = \b -> do
                           let h = headerHeight $ blockHeader b
                           st <- stateAtH bchState h
                           return $ [] <$ processBlock transitions CheckSignature b st
                       --
                       , appBlockGenerator = \h _ _ _ -> case nspecByzantine of
                           Just "InvalidBlock" -> do
                             return ([("XXX", NetAddrV6 (1,2,3,4) 4433)], [])
                           _ -> do
                             st <- stateAtH bchState h
                             let Just k = find (`Map.notMember` st) ["K_" ++ show (n :: Int) | n <- [1 ..]]
                             return ([(k, addr)], [])
                       --
                       , appCommitQuery    = SimpleQuery $ \_ -> return []
                       }
                     appCall = maybe mempty (callbackAbortAtH . Height) maxH
                 let cfg = defCfg :: Configuration Example
                 appCh <- newAppChans (cfgConsensus cfg)
                 runConcurrently
                   [ setNamespace "net"
                     $ startPeerDispatcher
                         (cfgNetwork cfg)
                         (createMockNode net addr)
                         addr
                         (connections netAddresses addr)
                         appCh
                         nullMempoolAny
                   , setNamespace "consensus"
                     $ runApplication (cfgConsensus cfg) nspecPrivKey appState appCall appCh
                   ]
             )
  where
    netAddresses = Map.fromList $ [ NetAddrV4 ha 2233 | ha <- [0 ..]] `zip` netNodeList
    connections  = case netTopology of
      Ring    -> connectRing
      All2All -> connectAll2All
    validatorSet = makeValidatorSetFromPriv [ pk | Just pk <- nspecPrivKey <$> netNodeList ]


executeSpec
  :: Maybe Int64                -- ^ Maximum height
  -> FilePath
  -> NetSpec NodeSpec
  -> IO [Connection 'RO Ed25519_SHA512 [(String,NetAddr)]]
executeSpec maxH prefix spec = do
  actions <- interpretSpec maxH prefix spec
  runConcurrently (snd <$> actions) `catch` (\Abort -> return ())
  return $ fst <$> actions
