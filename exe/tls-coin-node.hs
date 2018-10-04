{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Int
import Options.Applicative

import Codec.Serialise      (serialise)
import Control.Monad        (forever, void)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe           (isJust)
import Katip.Core           (LogEnv, showLS)
import Network.Simple.TCP   (accept, listen, recv)
import Network.Socket       (SockAddr(..))
import System.Environment   (getEnv)
import System.FilePath      ((</>))
import System.Random        (randomRIO)

import Thundermint.Blockchain.Interpretation
import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Crypto.Ed25519            (Ed25519_SHA512)
import Thundermint.Logger
import Thundermint.Mock
import Thundermint.Mock.Coin
import Thundermint.Mock.KeyList
import Thundermint.Mock.Types
import Thundermint.P2P.Consts
import Thundermint.P2P.Instances ()
import Thundermint.P2P.Network
    (getCredentialFromBuffer, getLocalAddress, realNetworkTls)
import Thundermint.Store
import Thundermint.Store.STM
import Thundermint.Store.SQLite

import qualified Control.Exception     as E
import qualified Data.Aeson            as JSON
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Map              as Map

----------------------------------------------------------------
-- Cmd line specification
----------------------------------------------------------------

data Opts = Opts
  { maxH              :: Maybe Int64
  , prefix            :: FilePath
  , delay             :: Int
  , doValidate        :: Bool
  , netInitialDeposit :: Integer
  , netInitialKeys    :: Int
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
   :: Opts -> [SockAddr] -> ValidatorSet Ed25519_SHA512 -> LogEnv -> BS.ByteString -> BS.ByteString
   -> NodeSpec -> IO (BlockStorage 'RO IO Alg [Tx], IO ())
interpretSpec Opts{..} netAddresses validatorSet logenv certPemBuf keyPemBuf NodeSpec{..} = do
  -- Allocate storage for node
  storage <- newSQLiteBlockStorage
    (maybe ":memory:" (prefix </>) nspecDbName)
    genesisBlock validatorSet
  hChain   <- blockchainHeight storage
  nodeAddr <- getLocalAddress
  let credential =  getCredentialFromBuffer certPemBuf keyPemBuf
  return
    ( makeReadOnly storage
    , runLoggerT "general" logenv $ do
        logger InfoS ("net Addresses: " <> showLS netAddresses) ()
        --
        bchState <- newBChState transitions
                  $ makeReadOnly (hoistBlockStorageRW liftIO storage)
        _        <- stateAtH bchState (next hChain)
        -- Create mempool
        let checkTx tx = do
              st <- currentState bchState
              return $ isJust $ processTx transitions (Height 1) tx st
        mempool <- newMempool checkTx
        cursor  <- getMempoolCursor mempool
        --
        let generator = forever $ do
              st <- currentState bchState
              let (off,n)  = nspecWalletKeys
                  privKeys = take n $ drop off privateKeyList
              transferActions delay (publicKey <$> take netInitialKeys privateKeyList) privKeys
                (void . pushTransaction cursor) st
        --
        acts <- runNode defCfg NodeDescription
          { nodeStorage         = hoistBlockStorageRW liftIO storage
          , nodeBchState        = bchState
          , nodeBlockChainLogic = transitions
          , nodeMempool         = mempool
          , nodeNetwork         = realNetworkTls credential (show thundermintPort)
          , nodeAddr            = nodeAddr
          , nodeInitialPeers    = netAddresses
          , nodeValidationKey   = nspecPrivKey
          , nodeCommitCallback = \case
              h | Just hM <- maxH
                , h > Height hM -> throwM Abort
                | otherwise     -> return ()
          }
        runConcurrently (generator : acts)
    )
  where
    genesisBlock = Block
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
    keyPem      <- BC8.pack <$> getEnv "KEY_PEM"
    certPem     <- BC8.pack <$> getEnv "CERT_PEM"

    let nodeSpec@NodeSpec{..}    = either error id $ JSON.eitherDecodeStrict' nodeSpecStr
        loggers = [ makeScribe s { scribe'path = fmap (prefix </>) (scribe'path s) }
                  | s <- nspecLogFile ]
    withLogEnv "TM" "DEV" loggers $ \logenv -> do
      let !validators'' = either error (fmap PrivValidator)
                        $ JSON.eitherDecodeStrict' ipMapPath  :: [PrivValidator Ed25519_SHA512]
          !validatorSet = makeValidatorSetFromPriv validators''
      runLoggerT "general" logenv $
        logger InfoS "Listening for bootstrap adresses" ()
      netAddresses <- waitForAddrs logenv
      (_,act) <- interpretSpec opts netAddresses validatorSet logenv certPem keyPem nodeSpec
      act `catch` (\Abort -> return ())
  where
    parser :: Parser Opts
    parser
      =    Opts
       <$> optional (option auto
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
       <*> option auto
           (  long "deposit"
           <> help "Initial deposit"
           <> metavar "N.N"
           )
       <*> option auto
           (  long "keys"
           <> help "Initial deposit"
           <> metavar "N"
           )


----------------------------------------------------------------
-- TCP server that listens for boostrap addresses
----------------------------------------------------------------

waitForAddrs :: LogEnv -> IO [SockAddr]
waitForAddrs logenv = E.handle allExc $ do
  addrs <- listen "*" "49999" $ \ (lsock, _addr) ->
    accept lsock $ \ (conn, _caddr) -> do
    mMsg <- recv conn 4096
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
    allExc (e::SomeException) = runLoggerT "boostrap" logenv $ do
      logger ErrorS (showLS e) ()
      E.throw e
