{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Codec.Serialise (Serialise, serialise)
import Data.Map                 (Map)
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString.Char8  as BC8
import qualified Data.Map               as Map
import System.Random (randomRIO)
import Text.Groom
import GHC.Generics (Generic)

import Thundermint.Blockchain.Types
import Thundermint.Blockchain.Interpretation
import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Crypto.Ed25519   (Ed25519_SHA512)
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.STM
import Thundermint.Store.SQLite
import Thundermint.Mock
import Thundermint.Mock.Coin


----------------------------------------------------------------
-- Keys
----------------------------------------------------------------

validatorKeys :: Map (Address Ed25519_SHA512) (PrivValidator Ed25519_SHA512)
validatorKeys = makePrivateValidators
  [ "2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y"
  , "4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL"
  , "3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS"
  , "D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd"
  ]


walletKeys :: Map (Address Ed25519_SHA512) (PrivValidator Ed25519_SHA512)
walletKeys = makePrivateValidators
  [ "6KpMDioUKSSCat18sdmjX7gvCNMGKBxf7wN8ZFAKBvvp"
  , "7KwrSxsYYgJ1ZcLSmZ9neR8GiZBCZp1C1XBuC41MdiXk"
  , "7thxDUPcx7AxDcz4eSehLezXGmRFkfwjeNUz9VUK6uyN"
  ]

----------------------------------------------------------------
-- Transactions
----------------------------------------------------------------
  
genesisBlock :: Block Ed25519_SHA512 [Tx]
genesisBlock = Block
  { blockHeader = Header
      { headerChainID     = "MONIES"
      , headerHeight      = Height 0
      , headerTime        = Time 0
      , headerLastBlockID = Nothing
      }
  , blockData       = [ Deposit (publicKey k) 1000 | PrivValidator k <- toList validatorKeys ]
  , blockLastCommit = Nothing
  }

----------------------------------------------------------------
--
----------------------------------------------------------------

run :: ValidatorSet Alg
    -> Maybe (PrivValidator Alg) -- Validator key
    -> Maybe (PrivValidator Alg) -- Wallet key
    -> IO ( AppState IO Alg [Tx]
          , Mempool IO Tx
          , Maybe (Async ())
          )
run validatorSet mPK mWK = do
  -- Create storage
  let toAddr = BC8.unpack
             . Base58.encodeBase58 Base58.bitcoinAlphabet
             . (\(Address a) -> a)
             . address . publicKey . validatorPrivKey
      dbName = case (mPK,mWK) of
        (Just pk, Just wk) -> "db/"++toAddr pk++"-"++toAddr wk
        (Just pk, Nothing) -> "db/"++toAddr pk++"-XXX"
        (Nothing, Just wk) -> "db/XXX-"++toAddr wk
        (Nothing, Nothing) -> ":memory:"
  storage <- newSQLiteBlockStorage dbName genesisBlock validatorSet
  -- Initialize application state
  coinState <- newBChState
    transitions
    (makeReadOnly storage)
  _ <- stateAtH coinState (Height 1)
  -- Transaction check for mempool
  let checkTx tx = do
        coin <- currentState coinState
        case processTx transitions (Height 1) tx coin of
          Nothing -> return False
          Just _  -> return True
  mempool <- newMempool checkTx
  ----------------------------------------------------------------
  -- Generate random transactions and put them into mempool
  txGen <- forM mWK $ \(PrivValidator wallet) -> do
    let myPubKey     = publicKey wallet
        otherWallets = filter (/= myPubKey)
                       [ publicKey w | PrivValidator w <- toList walletKeys]
        chooseWallet = do i <- randomRIO (0, length otherWallets - 1)
                          return (otherWallets !! i)
    cursor <- getMempoolCursor mempool
    async $ forever $ do
      -- Select destination
      output <- chooseWallet
      toSend <- randomRIO (1,100)
      -- Select sources
      CoinState{..} <- currentState coinState
      let inputs = findInputs toSend
                   [ (inp, n)
                   | (inp, (pk,n)) <- Map.toList unspentOutputs
                   , pk == myPubKey
                   ]
      let txs = TxSend { txInputs  = map fst inputs
                       , txOutputs = [ (output,toSend)
                                     , (myPubKey, sum (map snd inputs) - toSend)
                                     ]
                       }
          sig = signBlob wallet $ toStrict $ serialise txs
          tx  = Send myPubKey sig txs
      --
      pushTransaction cursor tx
      threadDelay 300e3

  ----------------------------------------------------------------
  propStorage <- newSTMPropStorage
  return
    ( AppState
        { appStorage        = storage
        , appPropStorage    = propStorage
          --
        , appValidationFun  = \hBlock txs -> do
            coin <- stateAtH coinState hBlock
            case processBlock transitions hBlock txs coin of
              Nothing -> return False
              Just _  -> return True
          --
        , appBlockGenerator = \hBlock -> do
            coin <- stateAtH coinState hBlock
            txs  <- peekNTransactions mempool Nothing
            return $ transactionsToBlock transitions hBlock coin txs
        , appCommitCallback = filterMempool mempool
          --
        , appValidator     = mPK
        , appValidatorsSet = validatorSet
        , appMaxHeight     = Just (Height 10)
        }
    , mempool
    , txGen
    )

main :: IO ()
main = do
  let validatorSet = makeValidatorSetFromPriv validatorKeys
      nodeSet      = Map.fromList
                   $ zip [0::Int ..]
                     [ (Just pk, Just pk) | pk <- toList validatorKeys ]
                   -- $ weave
                   --   [ (Nothing, Just wk) | wk <- toList walletKeys    ]
  net   <- newMockNet
  nodes <- sequence
    [ do (appSt,mempool,txGen) <- run validatorSet val wallet
         return (( createMockNode net "50000" addr
                 , map (,"50000") $ connectRing nodeSet addr
                 , appSt
                 , mempool
                 )
                , txGen)
    | (addr,(val,wallet)) <- Map.toList nodeSet
    ]
  --
  s:_ <- runNodeSet $ map fst nodes
  mapM_ (mapM_ cancel . snd) nodes
  return ()
  -- forM_ st $ \s -> do
  do putStrLn "==== BLOCKCHAIN ================================================"
     bs <- loadAllBlocks s
     forM_ bs $ \b -> do
       putStrLn $ groom $ blockHeader b
       putStrLn $ groom $ blockData b
       putStrLn "----------------"


loadAllBlocks :: Monad m => BlockStorage ro m alg a -> m [Block alg a]
loadAllBlocks storage = go (Height 0)
  where
    go h = retrieveBlock storage h >>= \case
      Nothing -> return []
      Just b  -> (b :) <$> go (next h)


weave :: [a] -> [a] -> [a]
weave (a:as) (b:bs) = a : b : weave as bs
weave as []         = as
weave [] bs         = bs

findInputs :: (Num i, Ord i) => i -> [(a,i)] -> [(a,i)]
findInputs tgt = go 0
  where go _ [] = []
        go acc ((tx,i):rest)
          | acc' >= tgt = [(tx,i)]
          | otherwise   = (tx,i) : go acc' rest
          where
            acc' = acc + i


----------------------------------------------------------------
-- Additional functions meant to be run from GHCi
----------------------------------------------------------------

pprCoinState :: CoinState -> IO ()
pprCoinState (CoinState outs) = do
  let balances = Map.fromListWith (+)
        [ (address pk, i)
        | (pk,i) <- toList outs
        ]
  mapM_ print $ Map.toList balances
  putStrLn $ "Σ = " ++ show (sum balances)

printCoinStateUpdates :: FilePath -> IO ()
printCoinStateUpdates dbName = do
  let validatorSet = makeValidatorSetFromPriv validatorKeys
  storage <- newSQLiteBlockStorage dbName genesisBlock validatorSet
  let step coin h = do
        Just Block{..} <- retrieveBlock storage h
        let Just coin' = processBlock transitions h blockData coin
        putStrLn ("==== " ++ show h ++ "================")
        pprCoinState coin'
        return coin'
  Height hMax <- blockchainHeight storage
  _ <- foldM step CoinState{ unspentOutputs = Map.empty} [Height h | h <- [0 .. hMax]]  
  closeBlockStorage storage
