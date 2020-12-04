{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Data.Maybe
import Data.Word
import Data.Foldable
import Data.List (unfoldr)
import Data.Map  (Map)
import Data.Text (Text)
import qualified Data.Aeson      as JSON
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Vector     as V
import System.Random (randoms,mkStdGen)
import Data.Yaml.Config (loadYamlSettings, requireEnv)
import Options.Applicative
import Servant.Server
import Servant.Server.Generic
import qualified Network.Wai.Handler.Warp as Warp
import GHC.Generics (Generic)

import HSChain.Control.Channels
import HSChain.Control.Util
import HSChain.Control.Class
import HSChain.Crypto
import HSChain.Examples.Coin
import HSChain.Logger
import HSChain.Network.TCP
import HSChain.Network.Types
import HSChain.PoW.Consensus
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import HSChain.PoW.Node (genericMiningLoop)
import HSChain.Store.Query
import HSChain.Types.Merkle.Types
import HSChain.Examples.Coin.API
import HSChain.Config

----------------------------------------------------------------
--
----------------------------------------------------------------

-- |Node's configuration.
data Cfg = Cfg
  { cfgPort    :: Word16
  , cfgPeers   :: [NetAddr]
  , cfgLog     :: [ScribeSpec]
  , cfgDB      :: Maybe FilePath
  , cfgWebAPI  :: Maybe Int
  , cfgMinerPK :: Maybe (PrivKey Alg)
  }
  deriving stock (Show, Generic)
  deriving (JSON.FromJSON) via SnakeCase (DropSmart (Config Cfg))


genesis :: Block Coin
genesis = Block
  { blockHeight = Height 0
  , blockTime   = Time 0
  , prevBlock   = Nothing
  , blockData   = Coin { coinData   = merkled []
                       , coinNonce  = 0
                       , coinTarget = Target $ 2^(256-16 :: Int)
                       }
  }

main :: IO ()
main = do
  -- Parse configuration
  Opts{..} <- customExecParser (prefs showHelpOnError)
            $ info (helper <*> parser)
              (  fullDesc
              <> header   "PoW node settings"
              <> progDesc ""
              )
  Cfg{..} <- loadYamlSettings optConfigPath [] requireEnv
  -- Acquire resources
  let net    = newNetworkTcp cfgPort
      netcfg = NodeCfg { nKnownPeers     = 3
                       , nConnectedPeers = 3
                       , initialPeers    = cfgPeers
                       }
  withConnection (fromMaybe "" cfgDB) $ \conn -> 
    withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv -> runCoinT logEnv conn $ evalContT $ do
      (db, bIdx, sView) <- lift $ coinStateView genesis
      c0  <- lift $ createConsensus db sView bIdx
      pow <- startNode netcfg net db c0
      -- report progress
      void $ liftIO $ forkIO $ do
        ch <- atomicallyIO (chainUpdate pow)
        forever $ do bh <- stateBH <$> awaitIO ch
                     print (bhHeight bh, bhBID bh)
                     print $ retarget bh
      -- Start web node
      forM_ cfgWebAPI $ \port -> do
        dict <- lift $ CoinT ask
        let run :: CoinT IO a -> Handler a
            run (CoinT m) = liftIO $ runReaderT m dict
        cforkLinkedIO $ Warp.run port $ genericServeT run $ coinServer (mempoolAPI pow)
      -- TX generation
      case optGenerate of
        [] -> pure ()
        _  -> do keys :: Map Text (PrivKey Alg) <- liftIO $ loadYamlSettings optGenerate [] requireEnv
                 cforkLinked $ txGeneratorLoop pow (toList keys)
      -- Mining loop
      forM_ cfgMinerPK $ \pk -> do
        cforkLinked $ genericMiningLoop
          (\st t txs -> let bh = stateBH st
                        in createCandidateBlock bh t <$> createCandidateBlockData pk st bh txs)
          pow
      -- Wait forever
      liftIO $ forever $ threadDelay maxBound

txGeneratorLoop
  :: (MonadReadDB m, MonadIO m)
  => PoW (CoinState m) -> [PrivKey Alg] -> m ()
txGeneratorLoop pow keyList = do
  forever $ do
    mtx <- generateTX keyVec keyMap
    forM_ mtx $ sinkIO (postTransaction (mempoolAPI pow))
    liftIO $ threadDelay 25e3
  where
    keyVec = V.fromList $ Map.keys keyMap
    keyMap = Map.fromList [ (publicKey k, k) | k <- keyList ]


----------------------------------------------------------------
--
----------------------------------------------------------------

data Opts = Opts
  { optConfigPath :: [FilePath]     -- ^ Paths to configuration
  , optGenerate   :: [FilePath]     -- ^ Paths to generator key ring 
  }

parser :: Parser Opts
parser = do
  optConfigPath <- some $ strArgument
    (  metavar "PATH"
    <> help  "Path to configuration"
    <> showDefault
    )
  optGenerate <- many $ strOption
    (  long "txgen"
    <> help "Configuration of TX generator"
    )
  return Opts{..}

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
