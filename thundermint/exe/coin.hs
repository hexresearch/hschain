{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Foldable
import Data.Int
import Data.Maybe             (isJust)
import Data.Monoid
import Options.Applicative

import Codec.Serialise      (serialise)
import Data.ByteString.Lazy (toStrict)

import System.FilePath ((</>))
import System.Random   (randomRIO)


import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Map              as Map

import Thundermint.Blockchain.Interpretation
import Thundermint.Blockchain.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Logger
import Thundermint.Run
import Thundermint.Mock.Coin
import Thundermint.Mock.KeyList
import Thundermint.Mock.Types
import Thundermint.P2P.Network hiding (Connection)
import Thundermint.Store
import Thundermint.Store.Internal.Query (Connection,openConnection)
import Thundermint.Store.Internal.BlockDB
import Thundermint.Store.STM


----------------------------------------------------------------
-- Generation of transactions
----------------------------------------------------------------

transferActions
  :: (MonadIO m)
  => Int                        -- Delay between transactions
  -> [PublicKey Alg]            -- List of possible addresses
  -> [PrivKey Alg]              -- Private key which we own
  -> (Tx -> m ())               -- push new transaction
  -> CoinState                  -- Current state of
  -> m ()
transferActions delay publicKeys privKeys push CoinState{..} = do
  -- Pick private key
  privK <- do i <- liftIO $ randomRIO (0, length privKeys - 1)
              return (privKeys !! i)
  let pubK = publicKey privK
  -- Pick public key to send data to
  target <- do i <- liftIO $ randomRIO (0, nPK - 1)
               return (publicKeys !! i)
  amount <- liftIO $ randomRIO (0,20)
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
  liftIO $ threadDelay (delay * 1000)
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
   :: Maybe Int64 -> FilePath -> Int
   -> NetSpec NodeSpec
   -> IO [(Connection, IO ())]
interpretSpec maxH prefix delay NetSpec{..} = do
  net   <- newMockNet
  -- Connection map
  forM (Map.toList netAddresses) $ \(addr, NodeSpec{..}) -> do
    -- Allocate storage for node
    conn   <- openConnection $ maybe ":memory:" (prefix </>) nspecDbName
    runDBT conn $ queryRW $ initializeBlockhainTables genesisBlock validatorSet
    hChain <- runDBT conn $ queryRO $ blockchainHeight storage
    -- Prepare logging
    let loggers = [ makeScribe s { scribe'path = fmap (prefix </>) (scribe'path s) }
                  | s <- nspecLogFile ]
    return
      ( conn
      , runDBT conn $ withLogEnv "TM" "DEV" loggers $ \logenv -> runLoggerT "general" logenv $ do
          (bchState,logic) <- logicFromFold transitions
          -- Transactions generator
          cursor  <- getMempoolCursor $ nodeMempool logic
          let generator = forever $ do
                st <- currentState bchState
                let (off,n)  = nspecWalletKeys
                    privKeys = take n $ drop off privateKeyList
                transferActions delay (publicKey <$> take netInitialKeys privateKeyList) privKeys
                  (void . pushTransaction cursor) st
          --
          acts <- runNode defCfg
            BlockchainNet
              { bchNetwork          = createMockNode net "50000" addr
              , bchLocalAddr        = (addr, "50000")
              , bchInitialPeers     = map (,"50000") $ connections netAddresses addr
              }
            NodeDescription
              { nodeValidationKey   = nspecPrivKey
              , nodeCommitCallback  = \case
                  h | Just hM <- maxH
                    , h > Height hM -> throwM Abort
                    | otherwise     -> return ()
              }
            logic
          runConcurrently (generator : acts)
      )
  where    
    netAddresses = Map.fromList $ [0::Int ..] `zip` netNodeList
    connections  = case netTopology of
      Ring    -> connectRing
      All2All -> connectAll2All
    validatorSet = makeValidatorSetFromPriv [ pk | Just pk <- nspecPrivKey <$> netNodeList ]
    --
    storage :: BlockStorage Alg [Tx]
    storage = blockStorage
    genesisBlock :: Block Alg [Tx]
    genesisBlock =  Block
      { blockHeader = Header
          { headerChainID        = "MONIES"
          , headerHeight         = Height 0
          , headerTime           = Time 0
          , headerLastBlockID    = Nothing
          , headerValidatorsHash = hash validatorSet
          }
      , blockData       = [ Deposit (publicKey pk) netInitialDeposit
                          | pk <- take netInitialKeys privateKeyList
                          ]
      , blockLastCommit = Nothing
      }

executeNodeSpec
  :: Maybe Int64 -> FilePath -> Int
  -> NetSpec NodeSpec
  -> IO [Connection]
executeNodeSpec maxH prefix delay spec = do
  actions <- interpretSpec maxH prefix delay spec
  runConcurrently (snd <$> actions) `catch` (\Abort -> return ())
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
    storage :: BlockStorage Alg [Tx]
    storage = blockStorage
    --
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

        forM_ storageList $ \c -> runDBT c $ checkStorage storage

        forM_ heights $ \h -> do
          -- Check that all blocks match!
          blocks <- forM storageList $ \c -> do
            mb <- runDBT c $ queryRO $ retrieveBlock storage h
            case mb of
              Nothing -> error ("Missing block at " <> show h)
              Just b  -> return b
          when (not $ allEqual blocks) $
            error ("Block mismatch!" <> show h <> "\n" <> show blocks)
          -- Check that validator set match
          vals <- forM storageList $ \c -> do
            mv <- runDBT c $ queryRO $ retrieveValidatorSet storage h
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
{-
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
-}
