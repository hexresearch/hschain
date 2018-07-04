{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
import Data.Int
import Data.Map       (Map)
import Data.Maybe     (fromMaybe)
import Network.Socket (ServiceName, SockAddr(..), getNameInfo, tupleToHostAddress)

import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types

--import Thundermint.Crypto
import Thundermint.Crypto.Ed25519 (Ed25519_SHA512, privateKey)
import Thundermint.Mock
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.STM

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base58 as Base58
import qualified Data.Map               as Map


----------------------------------------------------------------
--
----------------------------------------------------------------

makePrivateValidators'
  :: [(SockAddr ,BS.ByteString)]
  -> Map SockAddr (PrivValidator Ed25519_SHA512)
makePrivateValidators' keys = Map.fromList
  [ (sockAddr, PrivValidator pk)
  | (sockAddr, bs) <- keys
  , let pk = case Base58.decodeBase58 Base58.bitcoinAlphabet bs of
          Just x  -> privateKey x
          Nothing -> error "Incorrect Base58 encoding for bs"
  ]

validators :: Map SockAddr (PrivValidator Ed25519_SHA512)
validators = makePrivateValidators'
  [ (mkLocalHost 10001,"2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y")
  , (mkLocalHost 10002,"4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL")
  , (mkLocalHost 10003,"3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS")
  , (mkLocalHost 10004,"D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd")
  , (mkLocalHost 10005,"6KpMDioUKSSCat18sdmjX7gvCNMGKBxf7wN8ZFAKBvvp")
  , (mkLocalHost 10006,"7KwrSxsYYgJ1ZcLSmZ9neR8GiZBCZp1C1XBuC41MdiXk")
  , (mkLocalHost 10007,"7thxDUPcx7AxDcz4eSehLezXGmRFkfwjeNUz9VUK6uyN")
  ]
 where
  mkLocalHost = flip SockAddrInet (tupleToHostAddress (0x7f, 0, 0, 1))


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


getServiceName :: SockAddr -> IO Network.Socket.ServiceName
getServiceName addr = getPortOrError <$> getNameInfo [] False True addr
 where
  getPortOrError = fromMaybe (error "Port doesn't spacified") . snd

main :: IO ()
main = do
  let validatorSet = makeValidatorSetFromPriv validators
  nodes <- sequence
    [ do storage     <- newSTMBlockStorage genesisBlock validatorSet
         propStorage <- newSTMPropStorage
         serviceName <- getServiceName addr
         return ( realNetwork serviceName
                -- TODO: connectRing creates a network with very poor
                -- connectivity. For real network should be made more than one
                -- connection and probably peer exchange should be implemented.
                , connectAll2All validators addr
                , AppState
                    { appStorage        = storage
                    , appPropStorage    = propStorage
                    , appValidationFun  = const (return True)
                    , appBlockGenerator = do
                        Height h <- blockchainHeight storage
                        return $ h * 100
                    , appValidator     = Just val
                    , appValidatorsSet = validatorSet
                    , appMaxHeight     = Just (Height 3)
                    }
                , nullMempool
                )
         | (addr, val) <- Map.toList validators
         ]
  _ <- runNodeSet nodes
  return ()
