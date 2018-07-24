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
import Thundermint.Mock.KeyList
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
  , nspecLogFile     :: NodeLogs
  , nspecWalletKeys  :: (Int,Int)
  }
  deriving (Generic,Show)

data NetSpec = NetSpec
  { netNodeList       :: [NodeSpec]
  , netTopology       :: Topology
  , netInitialDeposit :: Integer
  , netInitialKeys    :: Int
  , netPrefix         :: Maybe String
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

transferActions
  :: Int                        -- Delay between transactions
  -> [PublicKey Alg]            -- List of possible addresses
  -> [PrivKey Alg]              -- Private key which we own
  -> (Tx -> IO ())              -- push new transaction
  -> CoinState                  -- Current state of
  -> IO ()
transferActions delay publicKeys privKeys push CoinState{..} = do
  -- Pick private key
  privK <- do i <- randomRIO (0, length privKeys - 1)
              return (privKeys !! i)
  let pubK = publicKey privK
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
    let makedir path = let (dir,_) = splitFileName path
                       in createDirectoryIfMissing True dir
    storage <- case nspecDbName of
      Nothing -> newSTMBlockStorage       genesisBlock validatorSet
      Just nm -> do
        let dbName = prefix </> nm
        makedir dbName
        newSQLiteBlockStorage dbName genesisBlock validatorSet
    -- Create dir for logs
    logFiles <- do
      let txt  = fmap (prefix </>) $ txtLog  nspecLogFile
          json = fmap (prefix </>) $ jsonLog nspecLogFile
      forM_ txt  makedir
      forM_ json makedir
      return $ NodeLogs txt json
    return
      ( makeReadOnly storage
      , runNode NodeDescription
          { nodeStorage         = storage
          , nodeBlockChainLogic = transitions
          , nodeNetworks        = createMockNode net "50000" addr
          , nodeInitialPeers    = map (,"50000") $ connections netAddresses addr
          , nodeValidationKey   = guard nspecIsValidator >> nspecPrivKey
          , nodeLogFile         = logFiles
          , nodeAction          = do let (off,n)  = nspecWalletKeys
                                         privKeys = take n $ drop off privateKeyList
                                     return $ transferActions delay
                                       (publicKey <$> take netInitialKeys privateKeyList)
                                       privKeys
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
      , blockData       = [ Deposit (publicKey pk) netInitialDeposit
                          | pk <- take netInitialKeys privateKeyList
                          ]
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
    work maxH prefix delay doValidate file = do
      blob <- BC8.readFile file
      spec <- case JSON.eitherDecodeStrict blob of
        Right s -> return s
        Left  e -> error e
      storageList <- executeNodeSpec maxH prefix delay spec
      when doValidate $ do
        -- If maxH is nothing code is not reachable
        let Just maximumH = maxH
            heights = map (Height . fromIntegral) [0 .. maximumH]
            allEqual []     = error "Empty list impossible!"
            allEqual (x:xs) = all (x==) xs
        forM_ heights $ \h -> do
          -- Check that all blocks match!
          blocks <- forM storageList $ \s -> do
            mb <- retrieveBlock s h
            case mb of
              Nothing -> error ("Missing block at " <> show h)
              Just b  -> return b
          when (not $ allEqual blocks) $
            error ("Block mismatch!" <> show h <> "\n" <> show blocks)
          -- Check that validator set match
          vals <- forM storageList $ \s -> do
            mv <- retrieveValidatorSet s h
            case (h,mv) of
              (Height 0, Nothing) -> return Nothing
              (_       , Just v ) -> return (Just v)
              _                   -> error "Invalid validator!"
          when (not $ allEqual vals) $
            error ("Validators mismatch!" <> show h)
          putStrLn $ show h ++ " - OK"
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
     <*> option auto
           (  long    "delay"
           <> metavar "N"
           <> help    "delay between transactions in ms"
           )
     <*> switch
           (  long "check-consensus"
           <> help "validate databases"
           )
     <*> argument str
         (  help "Specification file"
         <> metavar "JSON"
         )


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
