{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Int
import Data.Maybe               (isJust)
import Data.Monoid              ((<>))
import Data.List
import qualified Data.Aeson             as JSON
import qualified Data.ByteString.Char8  as BC8
import qualified Data.Map               as Map
import Options.Applicative
import System.FilePath

import GHC.Generics (Generic)

import Thundermint.Blockchain.App
import Thundermint.Blockchain.Types
import Thundermint.Blockchain.Interpretation
import Thundermint.Control
import Thundermint.Consensus.Types
import Thundermint.Crypto.Ed25519   (Ed25519_SHA512)
import Thundermint.Logger
import Thundermint.P2P
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Mock
import Thundermint.Mock.KeyVal

----------------------------------------------------------------
--
----------------------------------------------------------------

data NodeSpec = NodeSpec
  { nspecPrivKey     :: Maybe (PrivValidator Ed25519_SHA512)
  , nspecDbName      :: Maybe FilePath
  , nspecLogFile     :: [ScribeSpec]
  }
  deriving (Generic,Show)

data NetSpec = NetSpec
  { netNodeList     :: [NodeSpec]
  , netTopology     :: Topology
  , netPrefix       :: Maybe String
  }
  deriving (Generic,Show)

instance JSON.ToJSON   NodeSpec
instance JSON.ToJSON   NetSpec
instance JSON.FromJSON NodeSpec
instance JSON.FromJSON NetSpec


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
    storage  <- newBlockStorage prefix nspecDbName genesisBlock validatorSet
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
            , appBlockGenerator = \h -> do
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
  runConcurrently (snd <$> actions) `catch` (\Abort -> return ())
  return $ fst <$> actions

main :: IO ()
main
  = join . customExecParser (prefs showHelpOnError)
  $ info (helper <*> parser)
    (  fullDesc
    <> header   "Coin test program"
    <> progDesc ""
    )
  where
    work maxH prefix file = do
      blob <- BC8.readFile file
      spec <- case JSON.eitherDecodeStrict blob of
        Right s -> return s
        Left  e -> error e
      _storageList <- executeSpec maxH prefix spec
      return ()
    ----------------------------------------
    parser :: Parser (IO ())
    parser
      = pure work
     <*> optional (option auto
                    (  long    "max-h"
                    <> metavar "N"
                    <> help    "Maximum height"
                    ))
     <*> option str
          (  long    "prefix"
          <> value   "."
          <> metavar "PATH"
          <> help    "prefix for db & logs"
          )
     <*> argument str
         (  help "Specification file"
         <> metavar "JSON"
         )
