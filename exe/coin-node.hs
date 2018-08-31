{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Int
import Data.Monoid
import Options.Applicative

import Codec.Serialise      (serialise)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe           (fromMaybe)
import GHC.Generics         (Generic)
import Katip.Core           (LogEnv, showLS)
import Network.Simple.TCP   (accept, listen, recv)
import Network.Socket       (SockAddr(..), addrAddress, getAddrInfo, getNameInfo)
import System.Environment   (getEnv)
import System.FilePath      ((</>))
import System.IO.Unsafe     (unsafePerformIO)
import System.Random        (randomRIO)

import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Crypto.Ed25519    (Ed25519_SHA512)
import Thundermint.Logger
import Thundermint.Mock
import Thundermint.Mock.Coin
import Thundermint.P2P.Instances ()
import Thundermint.Mock.KeyList
import Thundermint.P2P.Network (realNetwork, getLocalAddress)
import Thundermint.Store

import qualified Control.Exception     as E
import qualified Data.Aeson            as JSON
import qualified Data.Aeson.Types      as JSON
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Map              as Map
import qualified Data.Text             as T

thundemintPort :: String
thundemintPort = "50000"

----------------------------------------------------------------
-- Generating node specification
----------------------------------------------------------------

data NodeSpec = NodeSpec
  { nspecPrivKey    :: Maybe (PrivValidator Ed25519_SHA512)
  , nspecDbName     :: Maybe FilePath
  , nspecLogFile    :: [ScribeSpec]
  , nspecWalletKeys :: (Int,Int)
  }
  deriving (Generic,Show)

instance JSON.ToJSON   NodeSpec
instance JSON.FromJSON NodeSpec

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
          , nodeNetworks        = realNetwork thundemintPort
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
  addrs <- listen "*" "49999" $ \ (lsock, _addr) ->
    accept lsock $ \ (conn, _caddr) -> do
      mMsg <- recv conn 4096
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

sa2Text :: SockAddr -> T.Text
sa2Text sa = T.pack
        $ fromMaybe "" mHN <> maybe "" (":"<>) mSN
     where (mHN, mSN) = unsafePerformIO $ getNameInfo [] True True sa

text2Sa :: T.Text -> SockAddr
text2Sa s = addrAddress $ head addrInfos
  where (hN, sN)  = T.breakOn ":" s
        mHN       = if T.null hN
                      then Nothing
                      else Just $ T.unpack hN
        mSN       = if T.null sN
                       then Just thundemintPort
                       else Just $ T.unpack $ T.tail sN
        addrInfos = unsafePerformIO $ getAddrInfo Nothing mHN mSN


instance JSON.ToJSON SockAddr where
  toJSON = JSON.String . sa2Text

instance JSON.ToJSONKey SockAddr where
  toJSONKey = JSON.toJSONKeyText sa2Text

instance JSON.FromJSONKey SockAddr where
  fromJSONKey = JSON.FromJSONKeyText text2Sa

instance JSON.FromJSON SockAddr where
  parseJSON (JSON.String s) = return $ text2Sa s
  parseJSON invalid         = JSON.typeMismatch "SockAddr" invalid

