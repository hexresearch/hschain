{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
import Control.Monad
import Control.Concurrent
import Codec.Serialise      (serialise)
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.Monoid
import qualified Data.Aeson             as JSON
import qualified Data.ByteString.Char8  as BC8
import qualified Data.Map               as Map
import System.Random    (randomRIO)
import System.Directory (createDirectoryIfMissing)
import System.FilePath  ((</>),splitFileName)
import GHC.Generics (Generic)
import Options.Applicative

import Thundermint.Blockchain.Types
import Thundermint.Blockchain.Interpretation
import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Crypto.Ed25519   (Ed25519_SHA512)
import Thundermint.Control
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.STM
import Thundermint.Store.SQLite
import Thundermint.Mock
import Thundermint.Mock.Coin

----------------------------------------------------------------
-- Generating node specification
----------------------------------------------------------------

data Topology = All2All
              | Ring
              deriving (Generic,Show)

data NodeSpec = NodeSpec
  { nspecPrivKey     :: Maybe (PrivValidator Ed25519_SHA512)
  , nspecIsValidator :: Bool
  , nspecDbName      :: Maybe FilePath
  , nspecLogFile     :: Maybe FilePath
  }
  deriving (Generic,Show)

data NetSpec = NetSpec
  { netNodeList :: [NodeSpec]
  , netTopology :: Topology
  , netGenesis  :: [Tx]
  , netPrefix   :: Maybe String
  }
  deriving (Generic,Show)

instance JSON.ToJSON   Topology
instance JSON.ToJSON   NodeSpec
instance JSON.ToJSON   NetSpec
instance JSON.FromJSON Topology
instance JSON.FromJSON NodeSpec
instance JSON.FromJSON NetSpec

----------------------------------------------------------------
-- Generation of transactions
----------------------------------------------------------------

transferActions :: Int -> [PublicKey Alg] -> PrivKey Alg
                -> (Tx -> IO ()) -> CoinState -> IO ()
transferActions delay publicKeys privK push CoinState{..} = do
  -- Pick public key to send data to
  target <- do i <- randomRIO (0, nPK - 1)
               return (publicKeys !! i)
  amount <- randomRIO (0,20)
  -- Create transaction
  let inputs = findInputs amount [ (inp, n)
                                 | (inp, (pk,n)) <- Map.toList unspentOutputs
                                 , pk == pubK
                                 ]
      tx     = TxSend { txInputs  = map fst inputs
                      , txOutputs = [ (target, amount)
                                    , (pubK  , sum (map snd inputs) - amount)
                                    ]
                      }
  push $ Send pubK (signBlob privK $ toStrict $ serialise tx) tx
  threadDelay (delay * 1000)
  where
    pubK = publicKey privK
    nPK  = length publicKeys


findInputs :: (Num i, Ord i) => i -> [(a,i)] -> [(a,i)]
findInputs tgt = go 0
  where go _ [] = []
        go acc ((tx,i):rest)
          | acc' >= tgt = [(tx,i)]
          | otherwise   = (tx,i) : go acc' rest
          where
            acc' = acc + i


----------------------------------------------------------------
--
----------------------------------------------------------------

interpretSpec
   :: Maybe Int -> FilePath -> Int
   -> NetSpec -> IO [(BlockStorage 'RO IO Alg [Tx], IO ())]
interpretSpec maxH prefix delay NetSpec{..} = do
  net   <- newMockNet
  -- Connection map
  forM (Map.toList netAddresses) $ \(addr, NodeSpec{..}) -> do
    -- Allocate storage for node
    storage <- case nspecDbName of
      Nothing -> newSTMBlockStorage       genesisBlock validatorSet
      Just nm -> do
        let dbName  = prefix </> nm
            (dir,_) = splitFileName dbName
        createDirectoryIfMissing True dir
        newSQLiteBlockStorage dbName genesisBlock validatorSet
    -- Create dir for logs
    logFile <- forM nspecLogFile $ \nm -> do
      let logName = prefix </> nm
          (dir,_) = splitFileName logName
      createDirectoryIfMissing True dir
      return logName
    return
      ( makeReadOnly storage
      , runNode NodeDescription
          { nodeStorage         = storage
          , nodeBlockChainLogic = transitions
          , nodeNetworks        = createMockNode net "50000" addr
          , nodeInitialPeers    = map (,"50000") $ connections netAddresses addr
          , nodeValidationKey   = guard nspecIsValidator >> nspecPrivKey
          , nodeLogFile         = logFile
          , nodeAction          = do PrivValidator pk <- nspecPrivKey
                                     return $ transferActions delay
                                       [ k | Deposit k _ <- netGenesis ]
                                       pk
          , nodeMaxH            = Height . fromIntegral <$> maxH
          }
      )
  where
    netAddresses = Map.fromList $ [0::Int ..] `zip` netNodeList
    connections  = case netTopology of
      Ring    -> connectRing
      All2All -> connectAll2All
    validatorSet = makeValidatorSetFromPriv [ pk | Just pk <- nspecPrivKey <$> netNodeList ]
    genesisBlock = Block
      { blockHeader = Header
          { headerChainID     = "MONIES"
          , headerHeight      = Height 0
          , headerTime        = Time 0
          , headerLastBlockID = Nothing
          }
      , blockData       = netGenesis
      , blockLastCommit = Nothing
      }

executeNodeSpec
  :: Maybe Int -> FilePath -> Int
  -> NetSpec -> IO [BlockStorage 'RO IO Alg [Tx]]
executeNodeSpec maxH prefix delay spec = do
  actions <- interpretSpec maxH prefix delay spec
  runConcurrently $ snd <$> actions
  return $ fst <$> actions

----------------------------------------------------------------
--
----------------------------------------------------------------

main :: IO ()
main
  = join . customExecParser (prefs showHelpOnError)
  $ info (helper <*> parser)
    (  fullDesc
    <> header   "Coin test program"
    <> progDesc ""
    )
  where
    work maxH prefix delay file = do
      blob <- BC8.readFile file
      spec <- case JSON.eitherDecodeStrict blob of
        Right s -> return s
        Left  e -> error e
      _ <- executeNodeSpec maxH prefix delay spec
      return ()
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
     <*> option auto
           (  long    "delay"
           <> metavar "N"
           <> help    "delay between transactions in ms"
           )
     <*> argument str
         (  help "Specification file"
         <> metavar "JSON"
         )


weave :: [a] -> [a] -> [a]
weave (a:as) (b:bs) = a : b : weave as bs
weave as []         = as
weave [] bs         = bs



----------------------------------------------------------------
-- Additional functions meant to be run from GHCi
----------------------------------------------------------------

pprCoinState :: CoinState -> IO ()
pprCoinState (CoinState outs) = do
  let balances = Map.fromListWith (+)
        [ (address pk, i)
        | (pk,i) <- toList outs
        ]
  mapM_ print $ Map.toList balances
  putStrLn $ "Σ = " ++ show (sum balances)

printCoinStateUpdates :: FilePath -> IO ()
printCoinStateUpdates dbName =
  withSQLiteBlockStorageRO dbName $ \(storage :: BlockStorage 'RO IO Alg [Tx]) -> do
    let step coin h = do
          Just Block{..} <- retrieveBlock storage h
          let Just coin' = processBlock transitions h blockData coin
          putStrLn ("==== " ++ show h ++ "================")
          pprCoinState coin'
          return coin'
    Height hMax <- blockchainHeight storage
    _ <- foldM step CoinState{ unspentOutputs = Map.empty} [Height h | h <- [0 .. hMax]]
    return ()
