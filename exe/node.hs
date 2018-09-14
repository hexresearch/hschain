{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
import Data.Aeson
    (eitherDecodeStrict')
import Data.Int
import Data.Maybe                (fromMaybe)
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
    -- , fdSocket
    , getAddrInfo
    , listen
    -- , setCloseOnExecIfNeeded
    , setSocketOption
    , socket
    )

import Data.Default.Class        (def)
import Network.Socket.ByteString (recv)
import System.Environment        (getEnv)
import qualified Control.Exception     as E
import qualified Data.ByteString.Char8 as BS8

import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.Crypto         (decodeBase58)
import Thundermint.Crypto.Ed25519 (Ed25519_SHA512, privateKey)
import Thundermint.Mock
import Thundermint.P2P.Consts
import Thundermint.P2P.Instances  ()
import Thundermint.P2P.Network    (realNetwork, getLocalAddress)
import Thundermint.Store
import Thundermint.Store.STM


----------------------------------------------------------------
--
----------------------------------------------------------------


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
  addr <- resolve "49999"
  E.bracket (open addr) close $ \ sock ->
    E.bracket (fst <$> accept sock) close $ \ conn -> do
      msg <- recv conn 4096
      either error return $ eitherDecodeStrict' msg
 where
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
        -- let fd = fdSocket sock
        -- TODO: commented to use network-2.6.*
        --       fix to provide on-close behavior
        -- setCloseOnExecIfNeeded fd
        listen sock 10
        return sock

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
  let net = realNetwork def (show thundermintPort)
  !ownAddr <- getLocalAddress
  !addrs <- waitForAddrs
  startNode net ownAddr addrs AppState
             { appStorage        = storage
             , appValidationFun  = \_ _ -> return True
             , appBlockGenerator = \_ -> do
                 Height h <- blockchainHeight storage
                 return $ h * 100
             , appCommitCallback = \case
                 h | h > Height 5 -> error "EJECT EJECT!!!"
                   | otherwise    -> return ()
             , appValidator        = Just val
             , appNextValidatorSet = \_ _ -> return validatorSet
             }
             nullMempool
  return ()
