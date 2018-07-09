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
import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Crypto.Ed25519   (Ed25519_SHA512)
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.STM
import Thundermint.Store.SQLite
import Thundermint.Mock


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

type Alg = Ed25519_SHA512

-- | Single transaction for transfer of coins
data TxSend = TxSend
  { txInputs  :: [(Hash Alg, Int)]
  , txOutputs :: [(PublicKey Alg, Integer)]
  }
  deriving (Show, Eq, Generic)
instance Serialise TxSend

data Tx
  = Deposit (PublicKey Alg) Integer
    -- ^ Deposit tokens to given key. Could only appear in genesis
    --   block
  | Send (PublicKey Alg) (Signature Alg) TxSend
    -- ^ Send coins to other addresses. Transaction must obey
    --   following invariants:
    --
    --   0. Signature must be valid
    --   1. All inputs must be owned by transaction issuer
    --   3. Inputs and outputs must be nonempty
    --   2. Sum of inputs must be equal to sum of outputs
  deriving (Show, Eq, Generic)
instance Serialise Tx



-- | State of coins in program-digestible format
--
--   Really we'll need to keep in DB to persist it.
data CoinState = CoinState
  { unspentOutputs :: Map (Hash Alg, Int) (PublicKey Alg, Integer)
    -- ^ Map of unspent outputs of transaction. It maps pair of
    --   transaction hash and output index to amount of coins stored
    --   there.
  }
  deriving (Show)

processDeposit :: Tx -> CoinState -> Maybe CoinState
processDeposit Send{}                _             = Nothing
processDeposit tx@(Deposit pk nCoin) CoinState{..} =
  return CoinState
    { unspentOutputs = Map.insert (txHash,0) (pk,nCoin) unspentOutputs
    }
    where
      txHash = hashBlob $ toStrict $ serialise tx


processTransaction :: Tx -> CoinState -> Maybe CoinState
processTransaction Deposit{} _ = Nothing
processTransaction transaction@(Send pubK sig txSend@TxSend{..}) CoinState{..} = do
  -- Signature must be valid
  guard $ verifyBlobSignature pubK (toStrict $ serialise txSend) sig
  -- Inputs and outputs are not null
  guard $ not $ null txInputs
  guard $ not $ null txOutputs
  -- Outputs are all positive
  forM_ txOutputs $ \(_,n) -> guard (n > 0)
  -- Inputs are owned Spend and generated amount match and transaction
  -- issuer have rights to funds
  inputs <- forM txInputs $ \i -> do
    (pk,n) <- Map.lookup i unspentOutputs
    guard $ pk == pubK
    return n
  guard (sum inputs == sum (map snd txOutputs))
  -- Update application state
  let txHash = hashBlob $ toStrict $ serialise transaction
  return CoinState
    { unspentOutputs =
        let spend txMap = foldl' (flip  Map.delete) txMap txInputs
            add   txMap = foldl'
                            (\m (i,out) -> Map.insert (txHash,i) out m)
                            txMap ([0..] `zip` txOutputs)
        in add $ spend unspentOutputs
    }

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

transitions :: BlockFold CoinState Tx [Tx]
transitions = BlockFold
  { processTx           = process
  , processBlock        = \h txs s0 -> foldM (flip (process h)) s0 txs
  , transactionsToBlock = id
  }
  where
    process (Height 0) t s = processDeposit t s <|> processTransaction t s
    process _          t s = processTransaction t s

----------------------------------------------------------------
-- Tracking of blockchain state
----------------------------------------------------------------

data BlockFold s tx a = BlockFold
  { processTx    :: Height -> tx -> s -> Maybe s
    -- ^ Try to process single transaction. Nothing indicates that
    --   transaction is invalid
  , processBlock :: Height -> a  -> s -> Maybe s
    -- ^ Try to process whole block
  , transactionsToBlock :: [tx] -> a
  }

-- | Wrapper for obtaining state of blockchain. It's quite limited
--   calls must be done with in nondecreasing height.
data BChState m s = BChState
  { currentState :: m s
  , stateAtH :: Height -> m s
  }

-- | Create block storage backed by MVar
newBChState
  :: s                          -- ^ Initial state before genesis block
  -> BlockFold s tx a           -- ^ Updating function
  -> BlockStorage 'RO IO alg a  -- ^ Store of blocks
  -> IO (BChState IO s)
newBChState s0 BlockFold{..} BlockStorage{..} = do
  state <- newMVar (Height 0, s0)
  let ensureHeight hBlk = do
        (st,flt) <- modifyMVar state $ \st@(h,s) ->
          case h `compare` hBlk of
            GT -> error "newBChState: invalid parameter"
            EQ -> return (st, (s,False))
            LT -> do Just Block{..} <- retrieveBlock h
                     case processBlock h blockData s of
                       Just st' -> return ((next h, st'), (st',True))
                       Nothing  -> error "OOPS! Blockchain is not valid!!!"
        case flt of
          True  -> ensureHeight hBlk
          False -> return st
  return BChState
    { currentState = withMVar state (return . snd)
    , stateAtH     = ensureHeight
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
    CoinState { unspentOutputs = Map.empty }
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
            case foldM (flip processTransaction) coin txs of
              Nothing -> return False
              Just _  -> return True
          --
        , appBlockGenerator = \hBlock -> do
            coin <- stateAtH coinState hBlock
            txs  <- peekNTransactions mempool Nothing
            let selectTx _ []     = []
                selectTx c (t:tx) = case processTransaction t c of
                                      Nothing -> selectTx c  tx
                                      Just c' -> t : selectTx c' tx
            return $ selectTx coin txs
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
