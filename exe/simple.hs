{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
import Data.Int
import Data.Map                 (Map)
import qualified Data.Map               as Map

import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types

import Thundermint.Crypto
import Thundermint.Crypto.Ed25519   (Ed25519_SHA512)
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.STM
import Thundermint.Mock


----------------------------------------------------------------
--
----------------------------------------------------------------

validators :: Map (Address Ed25519_SHA512) (PrivValidator Ed25519_SHA512)
validators = makePrivateValidators
  [ "2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y"
  , "4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL"
  , "3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS"
  , "D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd"
  , "6KpMDioUKSSCat18sdmjX7gvCNMGKBxf7wN8ZFAKBvvp"
  , "7KwrSxsYYgJ1ZcLSmZ9neR8GiZBCZp1C1XBuC41MdiXk"
  , "7thxDUPcx7AxDcz4eSehLezXGmRFkfwjeNUz9VUK6uyN"
  ]


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


main :: IO ()
main = do
  let validatorSet = makeValidatorSetFromPriv validators
  net   <- newMockNet
  nodes <- sequence
    [ do storage     <- newSTMBlockStorage genesisBlock
         propStorage <- newSTMPropStorage
         return ( createMockNode net "50000" addr
                , map (,"50000") $ connectRing validators addr
                , AppState
                    { appStorage        = storage
                    , appPropStorage    = propStorage
                    , appValidationFun  = const (return True)
                    , appBlockGenerator = do
                        Height h <- blockchainHeight storage
                        return $ h * 100
                    , appValidator     = val
                    , appValidatorsSet = validatorSet
                    , appMaxHeight     = Just (Height 3)
                    }
                )
         | (addr, val) <- Map.toList validators
         ]
  _ <- runNodeSet nodes
  return ()
