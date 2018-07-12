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
import qualified Data.Aeson             as JSON
import qualified Data.ByteString.Char8  as BC8
import qualified Data.Map               as Map
import System.Random (randomRIO)
import GHC.Generics (Generic)

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
  , netMaxH     :: Maybe Int
  }
  deriving (Generic,Show)

instance JSON.ToJSON   Topology
instance JSON.ToJSON   NodeSpec
instance JSON.ToJSON   NetSpec
instance JSON.FromJSON Topology
instance JSON.FromJSON NodeSpec
instance JSON.FromJSON NetSpec

transferActions :: [PublicKey Alg] -> PrivKey Alg
                -> (Tx -> IO ()) -> CoinState -> IO ()
transferActions publicKeys privK push CoinState{..} = do
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
  threadDelay 300e3
  where
    pubK = publicKey privK
    nPK  = length publicKeys


interpretSpec :: NetSpec -> IO [(BlockStorage 'RO IO Alg [Tx], IO ())]
interpretSpec NetSpec{..} = do
  net   <- newMockNet
  -- Connection map
  forM (Map.toList netAddresses) $ \(addr, NodeSpec{..}) -> do
    -- Allocate storage for node
    storage <- case nspecDbName of
      Nothing -> newSTMBlockStorage       genesisBlock validatorSet
      Just nm -> newSQLiteBlockStorage nm genesisBlock validatorSet
    --
    return
      ( makeReadOnly storage
      , runNode NodeDescription
          { nodeStorage         = storage
          , nodeBlockChainLogic = transitions
          , nodeNetworks        = createMockNode net "50000" addr
          , nodeInitialPeers    = map (,"50000") $ connections netAddresses addr
          , nodeValidationKey   = guard nspecIsValidator >> nspecPrivKey
          , nodeLogFile         = nspecLogFile
          , nodeAction          = do PrivValidator pk <- nspecPrivKey 
                                     return $ transferActions
                                       [ k | Deposit k _ <- netGenesis ]
                                       pk
          , nodeMaxH            = Height . fromIntegral <$> netMaxH
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


executeNodeSpec :: NetSpec -> IO [BlockStorage 'RO IO Alg [Tx]]
executeNodeSpec spec = do
  actions <- interpretSpec spec
  runConcurrently $ snd <$> actions
  return $ fst <$> actions

main :: IO ()
main = do
  blob <- BC8.readFile "spec/simple.json"
  spec <- case JSON.eitherDecodeStrict blob of
    Right s -> return s
    Left  e -> error e
  _ <- executeNodeSpec spec
  return ()

-- loadAllBlocks :: Monad m => BlockStorage ro m alg a -> m [Block alg a]
-- loadAllBlocks storage = go (Height 0)
--   where
--     go h = retrieveBlock storage h >>= \case
--       Nothing -> return []
--       Just b  -> (b :) <$> go (next h)


weave :: [a] -> [a] -> [a]
weave (a:as) (b:bs) = a : b : weave as bs
weave as []         = as
weave [] bs         = bs

findInputs :: (Num i, Ord i) => i -> [(a,i)] -> [(a,i)]
findInputs tgt = go 0
  where go _ [] = []
        go acc ((tx,i):rest)
          | acc' >= tgt = [(tx,i)]
          | otherwise   = (tx,i) : go acc' rest
          where
            acc' = acc + i


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
