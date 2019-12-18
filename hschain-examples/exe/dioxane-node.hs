{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Aeson             (FromJSON)
import Data.Monoid            ((<>))
import Data.Word
import Data.Yaml.Config       (loadYamlSettings, requireEnv)
import Options.Applicative
import GHC.Generics (Generic)

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Logger
import HSChain.Mock.Dioxane
import HSChain.Mock.Types
import HSChain.Run
import HSChain.Control
import HSChain.Store
import HSChain.Mock
import HSChain.Crypto         (PublicKey)
import HSChain.P2P            (generatePeerId)
import HSChain.P2P.Network    (newNetworkTcp)
import HSChain.Types

import qualified HSChain.P2P.Types as P2PT

----------------------------------------------------------------
--
----------------------------------------------------------------

type Tag = DioTag 1000 4
type BD  = BData Tag

data Opts = Opts
  { cmdConfigPath :: [FilePath]
  , optMaxH       :: Maybe Height
  }

data NodeCfg = NodeCfg
  { nodePort      :: Word16
  , nodeSeeds     :: [P2PT.NetAddr]
  , nodeMaxH      :: Maybe Height
  , nodeIdx       :: Int
  }
  deriving (Show,Generic)
instance FromJSON NodeCfg

main :: IO ()
main = do
  -- Parse CLI
  Opts{..} <- customExecParser (prefs showHelpOnError)
            $ info (helper <*> parser)
              (  fullDesc
              <> header   "Coin node settings"
              <> progDesc ""
              )
  -- Read config.
  --
  -- NOTE: later files take precedence
  nspec@NodeSpec{} :*: NodeCfg{..} :*: (cfg :: Configuration Example)
    <- loadYamlSettings (reverse cmdConfigPath) [] requireEnv
  -- Start node
  evalContT $ do
    -- Create network
    peerId <- generatePeerId
    let peerInfo = P2PT.PeerInfo peerId nodePort 0
        bnet     = BlockchainNet { bchNetwork      = newNetworkTcp peerInfo
                                 , bchInitialPeers = nodeSeeds
                                 }
    --
    (conn, logenv) <- allocNode nspec
    let run = runLoggerT logenv . runDBT conn
    lift $ run $ do
      (RunningNode{..},acts) <- interpretSpec @_ @_ @Tag
        (nspec :*: cfg :*: bnet)
        nodeIdx
        (maybe mempty callbackAbortAtH (optMaxH <|> nodeMaxH))
      logOnException $ runConcurrently acts

    
parser :: Parser Opts
parser = do
  optMaxH <- optional $ Height <$> option auto
    (  metavar "N"
    <> long "max-h"
    <> help "Maximum height"
    )
  cmdConfigPath <- some $ strArgument
    (  metavar "PATH"
    <> help  "Path to configuration"
    <> showDefault
    )
  return Opts{..}
