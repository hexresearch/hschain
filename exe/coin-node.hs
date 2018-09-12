{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Concurrent
import Codec.Serialise      (serialise)
import Data.ByteString.Lazy (toStrict)
import Data.Default.Class   (def)
import Data.Monoid
import Data.Int
import qualified Data.Aeson             as JSON
import qualified Data.ByteString.Char8  as BC8
import qualified Data.Map               as Map
import System.Environment        (getEnv)
import System.Random    (randomRIO)
import System.FilePath  ((</>))
import GHC.Generics (Generic)
import Katip.Core (showLS, LogEnv)
import Options.Applicative
import Network.Socket
    ( AddrInfo(..)
    , AddrInfoFlag(..)
    , SockAddr(..)
    , SocketOption(..)
    , SocketType(..)
    , accept
    , addrAddress
    , bind
    , close
    , defaultHints
    , fdSocket
    , getAddrInfo
    , listen
    , setCloseOnExecIfNeeded
    , setSocketOption
    , socket
    )
import Network.Socket.ByteString (recv)

import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Crypto.Ed25519   (Ed25519_SHA512)
import Thundermint.P2P.Network (realNetwork, getLocalAddress)
import Thundermint.Store
import Thundermint.Logger
import Thundermint.Mock
import Thundermint.Mock.KeyList
import Thundermint.Mock.Coin

import qualified Control.Exception     as E

thundemintPort :: String
thundemintPort = "50000"

----------------------------------------------------------------
-- Generating node specification
----------------------------------------------------------------

data NodeSpec = NodeSpec
  { nspecPrivKey     :: Maybe (PrivValidator Ed25519_SHA512)
  , nspecDbName      :: Maybe FilePath
  , nspecLogFile     :: [ScribeSpec]
  , nspecWalletKeys  :: (Int,Int)
  }
  deriving (Generic,Show)

instance JSON.ToJSON   NodeSpec
instance JSON.FromJSON NodeSpec

data Opts = Opts
  { maxH :: Maybe Int64
  , prefix :: FilePath
  , delay :: Int
  , doValidate :: Bool
  , netInitialDeposit :: Integer
  , netInitialKeys :: Int
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
   :: Opts -> [SockAddr] -> ValidatorSet Ed25519_SHA512 -> LogEnv
   -> NodeSpec -> IO (BlockStorage 'RO IO Alg [Tx], IO ())
interpretSpec Opts{..} netAddresses validatorSet logenv NodeSpec{..} = do
  -- Allocate storage for node
  storage <- newBlockStorage prefix nspecDbName genesisBlock validatorSet
  nodeAddr <- getLocalAddress
  return
    ( makeReadOnly storage
    , runLoggerT "general" logenv $
        runNode NodeDescription
          { nodeStorage         = hoistBlockStorageRW liftIO storage
          , nodeBlockChainLogic = transitions
          , nodeNetworks        = realNetwork def thundemintPort
          , nodeAddr            = nodeAddr
          , nodeInitialPeers    = netAddresses
          , nodeValidationKey   = nspecPrivKey
          , nodeAction          = do let (off,n)  = nspecWalletKeys
                                         privKeys = take n $ drop off privateKeyList
                                     return $ transferActions delay
                                       (publicKey <$> take netInitialKeys privateKeyList)
                                       privKeys
          , nodeCommitCallback = \case
              h | Just hM <- maxH
                , h > Height hM -> throwM Abort
                | otherwise     -> return ()
          }
    )
  where
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
      (_,act) <- interpretSpec opts netAddresses validatorSet logenv nodeSpec
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
  addr <- resolve "49999"
  addrs <- E.bracket (open addr) close $ \ sock ->
    E.bracket (fst <$> accept sock) close $ \ conn -> do
      msg <- recv conn 4096
      either fail return $ JSON.eitherDecodeStrict' msg
  runLoggerT "boostrap" logenv $ do
    logger InfoS ("Got " <> showLS addrs <> " boostrap addresses.") ()
    logger InfoS ("Stop listening") ()
  return addrs
 where
    allExc (e::SomeException) = runLoggerT "boostrap" logenv $ do
      logger ErrorS (showLS e) ()
      E.throw e
    resolve port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        -- If the prefork technique is not used,
        -- set CloseOnExec for the security reasons.
        let fd = fdSocket sock
        -- TODO: commented to use network-2.6.*
        --       fix to provide on-close behavior
        setCloseOnExecIfNeeded fd
        listen sock 10
        return sock
