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

import Codec.Serialise
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Word
import Options.Applicative
import System.IO

import HSChain.PoW.Consensus
import HSChain.PoW.Types
import qualified HSChain.POW as POWFunc
import HSChain.Types.Merkle.Types
import HSChain.Examples.Simple
import HSChain.Examples.Util
import HSChain.PoW.Node (runNode)
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
  kvBlockTimeInterval = Const (Time 10000)
  kvSolvePuzzle b0@GBlock{..} = do
    (maybeAnswer, _hash) <- liftIO $ POWFunc.solve [LBS.toStrict $ serialise $ blockWithoutNonce h0] powCfg
    case maybeAnswer of
      Nothing -> return Nothing
      Just answer -> do
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
      headerAndAnswer = BS.concat [answer, onlyHeader]


blockWithoutNonce :: GBlock (KV TestChainNewPow) f -> GBlock (KV TestChainNewPow) f
blockWithoutNonce block@GBlock{..} =
  block { blockData = blockData { kvNonce = BS.empty } }

defaultPOWConfig :: POWFunc.POWConfig
defaultPOWConfig = POWFunc.defaultPOWConfig

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


getHeaderTxsToMine :: (Default (Nonce cfg), KVConfig cfg)
          => BH (KV cfg) -> a -> ((Header (KV cfg), [()]), a)
getHeaderTxsToMine bh a = ((toHeader $ GBlock
    { blockHeight = succ $ bhHeight bh
    , blockTime   = Time 0
    , prevBlock   = Just $! bhBID bh
    , blockData   = KV { kvData = merkled []
                       , kvNonce = defaultValue
                       , kvTarget = retarget bh
                       }
    }, []), a)

getBlockToMine :: String -> Header (KV cfg) -> [()] -> IO (Maybe (Block (KV cfg)))
getBlockToMine val hdr _ = do
  return $ Just $ hdr
                  { blockData   = KV { kvData = merkled [ let Height h = blockHeight hdr
                                                          in (fromIntegral h, val)
                                                        ]
                                     , kvNonce = kvNonce $ blockData hdr
                                     , kvTarget = kvTarget $ blockData hdr
                                     }
                  }

----------------------------------------------------------------
-- Configuration
----------------------------------------------------------------

data Opts = Opts
  { cmdConfigPath :: [FilePath]
  , optMine       :: Bool
  , optNewPoW     :: Bool
  , optNodeName   :: String
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
  optNewPoW <- switch
    (  long "new-pow"
    <> help "use new PoW function instead of SHA256"
    )
  optNodeName <- strOption
    (  metavar "NODE"
    <> long "node-name"
    <> help "node name used in marking mined blocks"
    )
  return Opts{..}

runNodeAnyPoW :: forall cfg . (Show (Nonce cfg), Default (Nonce cfg), KVConfig cfg)
              => Opts -> Block (KV cfg) ->IO ()
runNodeAnyPoW Opts{..} genesisBlock = do
  runNode cmdConfigPath optMine genesisBlock kvViewStep getHeaderTxsToMine (getBlockToMine optNodeName) Map.empty

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

