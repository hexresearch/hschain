{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

import Codec.Serialise          (Serialise)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Int
import Data.Map                 (Map)
import Data.Maybe               (fromMaybe)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BC8
import qualified Data.Map               as Map
import qualified Katip

import Thundermint.Blockchain.App
import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Crypto.Ed25519   (Ed25519_SHA512, privateKey)
import Thundermint.Logger
import Thundermint.P2P
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.STM



----------------------------------------------------------------
--
----------------------------------------------------------------

data Base58DecodingError = Base58DecodingError
    deriving Show

instance Exception Base58DecodingError

fromBase58 :: BS.ByteString -> BS.ByteString
fromBase58 = fromMaybe (throw Base58DecodingError) . decodeBase58

-- FIXME: keys show be stored somewhere

validators :: Map BS.ByteString  (PrivValidator Ed25519_SHA512 Int64)
validators = Map.fromList
  [ n .= PrivValidator { validatorPrivKey  = privateKey $ fromBase58 n
                       , validateBlockData = const True
                       }
  | n <- [ "2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y"
         , "4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL"
         , "3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS"
         , "D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd"
         , "6KpMDioUKSSCat18sdmjX7gvCNMGKBxf7wN8ZFAKBvvp"
         , "7KwrSxsYYgJ1ZcLSmZ9neR8GiZBCZp1C1XBuC41MdiXk"
         , "7thxDUPcx7AxDcz4eSehLezXGmRFkfwjeNUz9VUK6uyN"
         ]
  ]
  where (.=) = (,)

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

----------------------------------------------------------------
--
----------------------------------------------------------------

makeValidatorSet
  :: (Foldable f, Crypto alg)
  => f (PrivValidator alg a) -> Map (Address alg) (Validator alg)
makeValidatorSet vals = Map.fromList
  [ ( address (publicKey (validatorPrivKey v))
    , Validator { validatorPubKey      = publicKey (validatorPrivKey v)
                , validatorVotingPower = 1
                }
    )
  | v <- toList vals
  ]

connectAll2All :: Ord addr => Map addr a -> addr -> [addr]
connectAll2All vals addr =
  [ a
  | a <- Map.keys vals
  , a < addr
  ]

connectRing :: Ord addr => Map addr a -> addr -> [addr]
connectRing vals addr =
  case Map.splitLookup addr vals of
    (_ , Nothing, _ ) -> []
    (va, Just _ , vb) -> case Map.lookupMin vb of
      Just (a,_) -> [a]
      Nothing    -> case Map.lookupMin va of
        Just (a,_) -> [a]
        Nothing    -> []


----------------------------------------------------------------
--
----------------------------------------------------------------

-- Start node which will now run consensus algorithm
startNode
  :: (Ord addr, Show addr, Crypto alg, Serialise a, a ~ Int64)
  => NetworkAPI sock addr
  -> [addr]
  -> PrivValidator alg a
  -> Map (Address alg) (Validator alg)
  -> Block alg a
  -> IO ()
startNode net addrs val valSet genesis = do
  -- Initialize logging
  scribe <- Katip.mkFileScribe
    ("logs/" ++ let Address nm = address $ publicKey $ validatorPrivKey val
                in BC8.unpack (encodeBase58 nm)
    ) Katip.DebugS Katip.V2
  logenv <- Katip.registerScribe "log" scribe Katip.defaultScribeSettings
        =<< Katip.initLogEnv "TM" "DEV"
  flip finally (Katip.closeScribes logenv) $ do
    -- Initialize block storage
    storage <- newSTMBlockStorage genesis
    appCh   <- newAppChans
    --
    let appState = AppState { appStorage        = hoistBlockStorageRW liftIO storage
                            , appChainID        = "TEST"
                            , appBlockGenerator = \st -> do
                                Height h <- blockchainHeight st
                                return $ h * 100
                            , appValidator     = val
                            , appValidatorsSet = valSet
                            , appMaxHeight     = Just (Height 3)
                            }
    -- Start P2P
    let netRoutine = runLoggerT "net" logenv
                   $ startPeerDispatcher net addrs appCh (makeReadOnly storage)
    withAsync netRoutine $ \_ -> do
      runLoggerT "consensus" logenv $ runApplication appState appCh


withAsyncs :: [IO a] -> ([Async a] -> IO b) -> IO b
withAsyncs ios function
  = recur ([],ios)
  where
    recur (as,[])   = function (reverse as)
    recur (as,i:is) = withAsync i $ \a -> recur (a:as, is)

main :: IO ()
main = do
  net <- newMockNet
  let validatorSet = makeValidatorSet validators
      actions = [ do let node  = createMockNode net addr
                         addrs = map (,"50000") $ connectRing validators addr
                     startNode node addrs val validatorSet genesisBlock
                | (addr, val) <- Map.toList validators
                ]
  withAsyncs actions $ mapM_ wait
