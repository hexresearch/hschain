{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
module Main where

import qualified Data.Aeson as JSON
import Codec.Serialise
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Data.Yaml.Config       (loadYamlSettings, requireEnv)
import Lens.Micro
import Options.Applicative
import GHC.Generics (Generic)
import System.IO

import HSChain.PoW.Consensus
import HSChain.PoW.Logger
import HSChain.PoW.P2P
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import qualified HSChain.POW as POWFunc
import HSChain.Network.TCP
import HSChain.Network.Types
import HSChain.Types.Merkle.Types
import HSChain.Examples.Simple
import HSChain.Examples.Util
import HSChain.Control.Class
import HSChain.Control.Channels
import HSChain.Control.Util
import HSChain.Crypto
import HSChain.Crypto.SHA


----------------------------------------------------------------
--
----------------------------------------------------------------

data TestChain

class Default a where
  -- |A safe value to instantiate somewhere.
  defaultValue :: a

instance Default Word64 where defaultValue = 0

instance KVConfig TestChain where
  type Nonce TestChain = Word64
  kvAdjustInterval = Const 200
  kvBlockTimeInterval  = Const (Time 1000)
  kvSolvePuzzle blk = case solved of
    blk' : _ -> return (Just blk')
    _ -> return Nothing
    where
      nonces = map (+ kvNonce (blockData blk)) [0..2^(16 :: Int) - 1]
      solved = [ blk'
               | nonce <- nonces
               , let blk' = blk { blockData = (blockData blk) { kvNonce = nonce } }
               , let hdr' = toHeader blk'
               , let tgt' = hash256AsTarget hdr'
               , tgt' <= tgt
               ]
      tgt = blockTargetThreshold blk
  kvCheckPuzzle hdr = return $ blockTargetThreshold hdr >= resultTgt
    where
      resultTgt = hash256AsTarget hdr

data TestChainNewPow

instance Default BS.ByteString where defaultValue = BS.empty

instance KVConfig TestChainNewPow where
  type Nonce TestChainNewPow = BS.ByteString
  kvAdjustInterval = Const 200
  kvBlockTimeInterval  = Const (Time 1000)
  kvSolvePuzzle b0@GBlock{..} = do
    maybeAnswerHash <- liftIO $ POWFunc.solve [LBS.toStrict $ serialise $ blockWithoutNonce h0] powCfg
    case maybeAnswerHash of
      Nothing -> return Nothing
      Just (answer, _hash) -> do
        let mined = b0 { blockData = blockData { kvNonce = answer } }
        return $ Just mined
    where
      h0 = toHeader b0
      powCfg = defaultPOWConfig
                       { POWFunc.powCfgTarget = targetInteger tgt }
      tgt = blockTargetThreshold b0
  kvCheckPuzzle hdr
    | blockHeight hdr == 0 && hdr == toHeader genesisNewPoW = return True
    | otherwise = do
      liftIO $ POWFunc.check onlyHeader answer hashOfSum powCfg
    where
      powCfg = defaultPOWConfig
                       { POWFunc.powCfgTarget = targetInteger tgt }
      tgt = blockTargetThreshold hdr
      onlyHeader = LBS.toStrict $ serialise $ blockWithoutNonce hdr
      answer = kvNonce $ blockData hdr
      Hash hashOfSum = hashBlob headerAndAnswer :: Hash SHA256
      headerAndAnswer = BS.concat [onlyHeader, answer]


blockWithoutNonce :: GBlock (KV TestChainNewPow) f -> GBlock (KV TestChainNewPow) f
blockWithoutNonce block@GBlock{..} =
  block { blockData = blockData { kvNonce = BS.empty } }

defaultPOWConfig :: POWFunc.POWConfig
defaultPOWConfig = POWFunc.defaultPOWConfig
  { POWFunc.powCfgClausesCount = 1 } -- all for speed!

genesis :: Block (KV TestChain)
genesis = GBlock
  { blockHeight = Height 0
  , blockTime   = Time 0
  , prevBlock   = Nothing
  , blockData   = KV { kvData     = merkled []
                     , kvNonce = 0
                     , kvTarget = Target $ 2^(256 :: Int) - 1
                     }
  }


genesisNewPoW :: Block (KV TestChainNewPow)
genesisNewPoW = GBlock
  { blockHeight = Height 0
  , blockTime   = Time 0
  , prevBlock   = Nothing
  , blockData   = KV { kvData     = merkled []
                     , kvNonce = ""
                     , kvTarget = Target $ 2^(256 :: Int) - 1
                     }
  }


mineBlock :: (Default (Nonce cfg), KVConfig cfg)
          => Time -> String -> BH (KV cfg) -> IO (Maybe (Block (KV cfg)))
mineBlock now val bh = do
  mine $ GBlock
    { blockHeight = succ $ bhHeight bh
    , blockTime   = now
    , prevBlock   = Just $! bhBID bh
    , blockData   = KV { kvData = merkled [ let Height h = bhHeight bh
                                            in (fromIntegral h, val)
                                          ]
                       , kvNonce = defaultValue
                       , kvTarget = retarget bh
                       }
    }

----------------------------------------------------------------
-- Configuration
----------------------------------------------------------------

data Opts = Opts
  { cmdConfigPath :: [FilePath]
  , optPrintBCH   :: Bool
  , optMine       :: Bool
  , optNewPoW     :: Bool
  }

parser :: Parser Opts
parser = do
  cmdConfigPath <- some $ strArgument
    (  metavar "PATH"
    <> help  "Path to configuration"
    <> showDefault
    )
  optPrintBCH <- switch
    (  long "print"
    <> help "Print blockchain"
    )
  optMine <- switch
    (  long "mine"
    <> help "Mine blocks"
    )
  optNewPoW <- switch
    (  long "new-pow"
    <> help "use new PoW function instead of SHA256"
    )
  return Opts{..}

data Cfg = Cfg
  { cfgPort  :: Word16
  , cfgPeers :: [NetAddr]
  , cfgLog   :: [ScribeSpec]
  , cfgStr   :: String
  , cfgMaxH  :: Maybe Height
  }
  deriving stock    (Show,Generic)
  deriving anyclass (JSON.FromJSON)

runNodeAnyPoW :: forall cfg . (Show (Nonce cfg), Default (Nonce cfg), KVConfig cfg)
              => Opts -> Block (KV cfg) ->IO ()
runNodeAnyPoW Opts{..} genesisBlock = do
  Cfg{..} <- loadYamlSettings (reverse cmdConfigPath) [] requireEnv
  --
  let netcfg = NetCfg { nKnownPeers     = 3
                      , nConnectedPeers = 3
                      }
  let net = newNetworkTcp cfgPort
  db <- inMemoryDB @_ @_ @(KV cfg)
  let s0 = consensusGenesis genesisBlock (viewKV (blockID genesisBlock))
  withLogEnv "" "" (map makeScribe cfgLog) $ \logEnv ->
    runLoggerT logEnv $ evalContT $ do
      pow' <- startNode netcfg net cfgPeers db s0
      let pow = pow'
                { sendNewBlock = \b -> do
                                 liftIO $ putStrLn $ "mined: "++show b
                                 (sendNewBlock pow') b
                }
      let printBCH = when optPrintBCH $ do
            c <- atomicallyIO $ currentConsensus pow
            let loop bh@BH{bhBID = bid} = do
                  liftIO $ print (cfgStr, bid, bhHeight bh)
                  liftIO . print =<< retrieveBlock db bid
                  maybe (return ()) loop $ bhPrevious bh
            loop (c ^. bestHead . _1)
      lift $ flip onException printBCH $ do
        upd <- atomicallyIO $ chainUpdate pow
        let doMine = do
              c   <- atomicallyIO $ currentConsensus pow
              now <- getCurrentTime
              let bh = c ^. bestHead . _1
              case cfgMaxH of
                Just h -> liftIO $ when (bhHeight bh > h) $ forever $ threadDelay maxBound
                Nothing -> return ()
              maybeB <- liftIO $ mineBlock now cfgStr bh
              case maybeB of
                Just b -> sendNewBlock pow b >>= \case
                                                Right () -> return ()
                                                Left  e  -> error $ show e
                Nothing -> doMine
        let loop tid = do
              (bh,_) <- awaitIO upd
              Just b <- retrieveBlock db (bhBID bh)
              liftIO $ print ( blockHeight b
                             , blockTime b
                             , bhBID bh
                             , kvTarget $ blockData b
                             , merkleValue $ kvData $ blockData b
                             )
              liftIO $ mapM_ killThread tid
              loop =<< if optMine then Just <$> fork doMine else return Nothing
        --
        loop =<< if optMine then Just <$> fork doMine else return Nothing



main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  -- Parse CLI & read config
  opts@Opts{..} <- customExecParser (prefs showHelpOnError)
            $ info (helper <*> parser)
              (  fullDesc
              <> header   "PoW node settings"
              <> progDesc ""
              )
  if optNewPoW
    then runNodeAnyPoW opts genesisNewPoW
    else runNodeAnyPoW opts genesis
        -- -- t <- liftIO $ negate . log <$> randomRIO (0.5, 2)
        -- -- liftIO $ threadDelay $ round (1e6 * t :: Double)
        -- --
        -- when optMine $ do
        --   c   <- atomicallyIO $ currentConsensus pow
        --   now <- getCurrentTime
        --   let bh = c ^. bestHead . _1
        --       b  = mineBlock now cfgStr bh
        --   liftIO $ print (blockHeight b, blockTime b)
        --   sendNewBlock pow b >>= \case
        --     Right () -> return ()
        --     Left  e  -> error $ show e
