{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
import Data.Aeson
    (FromJSON, ToJSON, ToJSONKey, Value(..), eitherDecodeStrict', parseJSON, toJSON, toJSONKey)
import Data.Aeson.Types
    (FromJSONKey, FromJSONKeyFunction(..), fromJSONKey, toJSONKeyText, typeMismatch)
import Data.Int
import Data.Maybe         (fromMaybe)
import Data.Monoid        ((<>))
import Network.Simple.TCP (accept, listen, recv)
import Network.Socket     (SockAddr(..), addrAddress, getAddrInfo, getNameInfo)
import System.Environment (getEnv)
import System.IO.Unsafe   (unsafePerformIO)

import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types

import Thundermint.Crypto         (decodeBase58)
import Thundermint.Crypto.Ed25519 (Ed25519_SHA512, privateKey)
import Thundermint.Mock
import Thundermint.P2P.Network    (realNetwork)
import Thundermint.Store
import Thundermint.Store.STM


import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text             as T

----------------------------------------------------------------
--
----------------------------------------------------------------

thundemintPort :: String
thundemintPort = "50000"

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


instance ToJSON SockAddr where
  toJSON = String . sa2Text

instance ToJSONKey SockAddr where
  toJSONKey = toJSONKeyText sa2Text

instance FromJSONKey SockAddr where
  fromJSONKey = FromJSONKeyText text2Sa

instance FromJSON SockAddr where
  parseJSON (String s) = return $ text2Sa s
  parseJSON invalid    = typeMismatch "SockAddr" invalid

genesisBlock :: Block Ed25519_SHA512 Int64
genesisBlock = Block
  { blockHeader = Header
      { headerChainID     = "TEST"
      , headerHeight      = Height 0
      , headerTime        = Time 0
      , headerLastBlockID = Nothing
      }
  , blockData       = 0
  , blockLastCommit = Nothing
  }

waitForAddrs :: IO [SockAddr]
waitForAddrs = do
  listen "*" "49999" $ \ (lsock, _addr) ->
    accept lsock $ \ (conn, _caddr) -> do
      mMsg <- recv conn 4096
      case mMsg of
        Nothing  -> fail "Connection closed by peer."
        Just msg -> either fail return $ eitherDecodeStrict' msg

main :: IO ()
main = do
  selfPrivKeyStr <- BS8.pack <$> getEnv "THUNDERMINT_NODE_KEY"
  ipMapPath      <- BS8.pack <$> getEnv "THUNDERMINT_KEYS"
  let !validators'' = either error (fmap PrivValidator)
                   $ eitherDecodeStrict' ipMapPath  :: [PrivValidator Ed25519_SHA512]
      validatorSet = makeValidatorSetFromPriv validators''
      !val          = PrivValidator
                   $ privateKey
                   $ fromMaybe (error "Invalid base58 encoding")
                   $ decodeBase58
                   $ head $ BS8.lines selfPrivKeyStr
  storage     <- newSTMBlockStorage genesisBlock validatorSet
  propStorage <- newSTMPropStorage
  let net = realNetwork thundemintPort
  !addrs <- waitForAddrs
  startNode net addrs AppState
             { appStorage        = storage
             , appPropStorage    = propStorage
             , appValidationFun  = \_ _ -> return True
             , appBlockGenerator = \_ -> do
                 Height h <- blockchainHeight storage
                 return $ h * 100
             , appCommitCallback = \case
                 h | h > Height 5 -> error "EJECT EJECT!!!"
                   | otherwise    -> return ()
             , appValidator     = Just val
             , appValidatorsSet = validatorSet
             }
             nullMempool
  return ()
