{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Int
import Data.Monoid
import Options.Applicative

import Codec.Serialise      (serialise)
import Control.Monad        (forever, void)
import Data.ByteString.Lazy (toStrict)
import Katip.Core           (showLS, sl)
import Network.Simple.TCP   (accept, listen, recv, closeSock)
import Network.Socket       (SockAddr(..), PortNumber)
import System.Environment   (getEnv)
import System.FilePath      ((</>))
import System.Random        (randomRIO)

import Thundermint.Blockchain.Interpretation
import Thundermint.Blockchain.Internal.Engine.Types
import Thundermint.Blockchain.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Crypto.Ed25519            (Ed25519_SHA512)
import Thundermint.Logger
import Thundermint.Run
import Thundermint.Store
import Thundermint.Store.Internal.Query (Connection, openConnection)
import Thundermint.Store.Internal.BlockDB
import Thundermint.Mock.Coin
import Thundermint.Mock.KeyList
import Thundermint.Mock.Types
import Thundermint.P2P.Consts
import Thundermint.P2P.Instances ()
import Thundermint.P2P.Network               ( getLocalAddress, realNetwork
                                             , getCredentialFromBuffer, realNetworkTls)

import qualified Control.Exception     as E
import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Map              as Map

----------------------------------------------------------------
-- Cmd line specification
----------------------------------------------------------------
data Opts = Opts
  { maxH              :: Maybe Int64
  , listenPort        :: PortNumber
  , prefix            :: FilePath
  , delay             :: Int
  , doValidate        :: Bool
  , netInitialDeposit :: Integer
  , netInitialKeys    :: Int
  , optTls            :: Bool
  }

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
  :: Opts
  -> ValidatorSet Ed25519_SHA512
  -> BlockchainNet SockAddr
  -> NodeSpec
  -> LoggerT IO (Connection, LoggerT IO ())
interpretSpec Opts{..} validatorSet net NodeSpec{..} = do
  -- Allocate storage for node
  conn   <- openConnection (maybe ":memory:" (prefix </>) nspecDbName)
  runDBT conn $ queryRW $ initializeBlockhainTables genesisBlock validatorSet
  hChain <- runDBT conn $ queryRO $ blockchainHeight storage
  return
    ( conn
    , runDBT conn $ do
        (bchState,logic) <- logicFromFold transitions
        -- Transactions generator
        cursor  <- getMempoolCursor $ nodeMempool logic
        let generator = forever $ do
              st <- currentState bchState
              let (off,n)  = nspecWalletKeys
                  privKeys = take n $ drop off privateKeyList
              transferActions delay (publicKey <$> take netInitialKeys privateKeyList) privKeys
                (void . pushTransaction cursor) st
        acts <- runNode defCfg net
          NodeDescription
            { nodeValidationKey   = nspecPrivKey
            , nodeCommitCallback  = \h -> do
                -- Update state
                st <- stateAtH bchState (succ h)
                descendNamespace "coin" $ logger InfoS "State size" (sl "utxo" (Map.size (unspentOutputs st)))
                case maxH of
                  Just hM | h > Height hM -> throwM Abort
                  _                       -> return ()
            }
          logic
        runConcurrently (generator : acts)
    )
  where
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

----------------------------------------------------------------
--
----------------------------------------------------------------

main :: IO ()
main = do
    opts@Opts{..} <- customExecParser (prefs showHelpOnError)
                  $ info (helper <*> parser)
                    (  fullDesc
                    <> header   "Coin test program"
                    <> progDesc ""
                    )
    nodeSpecStr <- BC8.pack <$> getEnv "THUNDERMINT_NODE_SPEC"
    ipMapPath   <- BC8.pack <$> getEnv "THUNDERMINT_KEYS"
    let nodeSpec@NodeSpec{..} = either error id $ JSON.eitherDecodeStrict' nodeSpecStr
        loggers = [ makeScribe s { scribe'path = fmap (prefix </>) (scribe'path s) }
                  | s <- nspecLogFile
                  ]
    withLogEnv "TM" "DEV" loggers $ \logenv -> runLoggerT "Coin" logenv $ do
      let !validatorSet = makeValidatorSetFromPriv
                        $ (id @[PrivValidator Ed25519_SHA512])
                        $ either error (fmap PrivValidator)
                        $ JSON.eitherDecodeStrict' ipMapPath
      logger InfoS "Listening for bootstrap adresses" ()
      nodeAddr     <- liftIO getLocalAddress
      netAddresses <- waitForAddrs
      logger InfoS ("net Addresses: " <> showLS netAddresses) ()
      netAPI <- case optTls of
        False -> return $ realNetwork (show listenPort)
        True  -> liftIO $ do
          keyPem  <- BC8.pack <$> getEnv "KEY_PEM"
          certPem <- BC8.pack <$> getEnv "CERT_PEM"
          let credential = getCredentialFromBuffer certPem keyPem
          return $ realNetworkTls credential (show thundermintPort)
      let net = BlockchainNet
                   { bchNetwork          = netAPI
                   , bchLocalAddr        = nodeAddr
                   , bchInitialPeers     = netAddresses
                   }
      (_,act) <- interpretSpec opts validatorSet net nodeSpec
      act `catch` (\Abort -> return ())
  where
    parser :: Parser Opts
    parser = do
      maxH <- optional $ option auto
        (  long    "max-h"
        <> metavar "N"
        <> help    "Maximum height"
        )
      listenPort <- option auto
        (  long    "listen-port"
        <> value thundermintPort
        <> metavar "PORT"
        <> help    ("listening port (default " <> show thundermintPort <> ")")
        )
      prefix <- option str
        (  long    "prefix"
        <> value   ""
        <> metavar "PATH"
        <> help    "prefix for db & logs"
        )
      delay <- option auto
        (  long    "delay"
        <> metavar "N"
        <> help    "delay between transactions in ms"
        )
      doValidate <- switch
        (  long "check-consensus"
        <> help "validate databases"
        )
      netInitialDeposit <- option auto
        (  long "deposit"
        <> help "Initial deposit"
        <> metavar "N.N"
        )
      netInitialKeys <- option auto
        (  long "keys"
        <> help "Initial deposit"
        <> metavar "N"
        )
      optTls <- switch
        (  long "tls"
        <> help "Use TLS for node connection"
        )
      pure Opts{..}


----------------------------------------------------------------
-- TCP server that listens for boostrap addresses
----------------------------------------------------------------

waitForAddrs :: LoggerT IO [SockAddr]
waitForAddrs = LoggerT $ ReaderT $ \(_,logenv) -> E.handle (allExc logenv) $ do
  addrs <- listen "0.0.0.0" "49999" $ \ (lsock, _addr) ->
    accept lsock $ \ (conn, _caddr) -> do
    mMsg <- recv conn 4096
    closeSock conn
    runLoggerT "boostrap" logenv $ do
      logger InfoS ("accept this: " <> showLS mMsg) ()
    case mMsg of
        Nothing  -> fail "Connection closed by peer."
        Just msg -> either fail return $ JSON.eitherDecodeStrict' msg
  runLoggerT "boostrap" logenv $ do
    logger InfoS ("Got " <> showLS addrs <> " boostrap addresses.") ()
    logger InfoS "Stop listening" ()
  return addrs
  where
    allExc logenv (e::SomeException) = runLoggerT "boostrap" logenv $ do
      logger ErrorS (showLS e) ()
      E.throw e
