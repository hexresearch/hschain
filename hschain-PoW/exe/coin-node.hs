{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
module Main where

import Codec.Serialise
import Control.Concurrent (threadDelay, killThread, forkIO)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Word
import Data.Yaml.Config (loadYamlSettings, requireEnv)
import Options.Applicative
import System.IO
import Katip (LogEnv, Namespace)
import GHC.Generics (Generic)

import HSChain.PoW.Consensus
import HSChain.PoW.Types
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.Network.TCP
import HSChain.Logger
import HSChain.Control.Class
import HSChain.Control.Util
import HSChain.Control.Channels
import HSChain.Types.Merkle.Types
import HSChain.Examples.Coin
import HSChain.Examples.Util
import HSChain.PoW.Node (blockDatabase,inMemoryView,Cfg(..))
import HSChain.Crypto
import HSChain.Crypto.SHA
import HSChain.Store.Query


----------------------------------------------------------------
-- Monad for working with PoW
----------------------------------------------------------------

data CoinDict = CoinDict
  { dictLogEnv    :: !LogEnv
  , dictNamespace :: !Namespace
  , dictConn      :: !(Connection 'RW)
  }
  deriving (Generic)

newtype CoinT m a = CoinT (ReaderT CoinDict m a)
  deriving newtype ( Functor,Applicative,Monad,MonadIO
                   , MonadCatch,MonadThrow,MonadMask,MonadFork)
  deriving (MonadLogger)          via LoggerByTypes  (ReaderT CoinDict m)
  deriving (MonadDB, MonadReadDB) via DatabaseByType (ReaderT CoinDict m)

runCoinT :: LogEnv -> Connection 'RW -> CoinT m a -> m a
runCoinT logenv conn (CoinT act) = runReaderT act (CoinDict logenv mempty conn)

----------------------------------------------------------------
--
----------------------------------------------------------------

genesis :: Block Coin
genesis = GBlock
  { blockHeight = Height 0
  , blockTime   = Time 0
  , prevBlock   = Nothing
  , blockData   = Coin { coinData   = merkled []
                       , coinNonce  = 0
                       , coinTarget = Target $ 2^(256-10 :: Int)
                       }
  }

main :: IO ()
main = do
  -- Parse configuration
  opts@Opts{..} <- customExecParser (prefs showHelpOnError)
                 $ info (helper <*> parser)
                   (  fullDesc
                   <> header   "PoW node settings"
                   <> progDesc ""
                   )
  Cfg{..} <- loadYamlSettings cmdConfigPath [] requireEnv
  -- Acquire resources
  let net    = newNetworkTcp cfgPort
      netcfg = NetCfg { nKnownPeers     = 3
                      , nConnectedPeers = 3
                      }
  let sView = inMemoryView (\_ -> Just) () (blockID genesis)
  withConnection (fromMaybe "" cfgDB) $ \conn -> 
    withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv -> runCoinT logEnv conn $ evalContT $ do
      db   <- lift $ blockDatabase genesis
      bIdx <- lift $ buildBlockIndex db
      c0   <- lift $ createConsensus db bIdx sView
      pow  <- startNode netcfg net cfgPeers db c0
      liftIO $ forkIO $ do
        ch <- atomicallyIO (chainUpdate pow)
        forever $ do (bh,_) <- awaitIO ch
                     print $ asHeader bh
                     print $ retarget bh
      lift $ miningLoop pow optMine

miningLoop :: MonadFork m => PoW s m Coin -> Bool -> m x
miningLoop _   False = liftIO $ forever $ threadDelay maxBound
miningLoop pow True  = do
  start =<< atomicallyIO (chainUpdate pow)
  where
    --
    start ch = do
      c <- atomicallyIO $ currentConsensus pow
      let (bh, st, _) = _bestHead c
      loop ch =<< mine bh st
    --
    loop ch tid = do
      (bh, st) <- awaitIO ch
      liftIO $ killThread tid
      loop ch =<< mine bh st
    --
    mine bh st = fork $ do
      t <- getCurrentTime
      let blk :: Block Coin
          blk = GBlock { blockHeight = succ $ bhHeight bh
                       , blockTime   = t
                       , prevBlock   = Just $ bhBID bh
                       , blockData   = Coin { coinData   = merkled []
                                            , coinNonce  = 0
                                            , coinTarget = retarget bh
                                            }
                       }
      let find b = (fst <$> adjustPuzzle b) >>= \case
            Just b' -> return b'
            Nothing -> do t' <- getCurrentTime
                          find b { blockTime = t' }
      find blk >>= sendNewBlock pow >>= \case
        Right () -> return ()
        Left  e  -> liftIO $ throwM e


----------------------------------------------------------------
--
----------------------------------------------------------------

data Opts = Opts
  { cmdConfigPath :: [FilePath] -- ^ Path to configuration
  , optMine       :: Bool       -- ^ Whether to mine blocks
  }

parser :: Parser Opts
parser = do
  cmdConfigPath <- some $ strArgument
    (  metavar "PATH"
    <> help  "Path to configuration"
    <> showDefault
    )
  optMine <- switch
    (  long "mine"
    <> help "Mine blocks"
    )
  return Opts{..}
