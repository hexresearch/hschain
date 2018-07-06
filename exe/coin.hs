{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Codec.Serialise (Serialise, serialise)
import Data.Map                 (Map)
import Data.Set                 (Set)
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.List
import qualified Data.Map               as Map
import qualified Data.Set               as Set
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


processTx :: Tx -> CoinState -> Maybe CoinState
processTx Deposit{} _ = Nothing
processTx transaction@(Send pubK sig txSend@TxSend{..}) CoinState{..} = do
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



----------------------------------------------------------------
--
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


run :: ValidatorSet Alg
    -> Maybe (PrivValidator Alg) -- Validator key
    -> Maybe (PrivValidator Alg) -- Wallet key
    -> IO ( AppState IO Alg [Tx]
          , Mempool IO Tx
          )
run validatorSet mPK mWK = do
  storage   <- newSTMBlockStorage genesisBlock validatorSet
  -- Initialize application state
  coinState <- newMVar ( Height 0
                       , CoinState { unspentOutputs = Map.empty })
  -- Transaction check for mempool
  let checkTx tx = withMVar coinState $ \(h,coin) -> do
        case processTx tx coin of
          Nothing -> return False
          Just _  -> return True
  mempool <- newMempool checkTx
  -- Ensure that we're prepared to handle block at given height H.
  let ensureHeight hTgt = do
        (c,flt) <- modifyMVar coinState $ \st@(h,coin) -> do
          case h `compare` hTgt of
            GT -> error "Impossible"
            EQ -> return (st,(coin,False))
            LT -> do Just Block{..} <- retrieveBlock storage h
                     let process c tx
                           | h == Height 0 = processTx tx c <|> processDeposit tx c
                           | otherwise     = processTx tx c
                     print (hTgt,h,fmap validatorPrivKey mPK)
                     case foldM process coin blockData of
                       Just coin' -> return ((next h, coin'), (coin',True))
                       Nothing    -> error "OOPS! Blockchain is not valid!!!"
        case flt of
          True  -> filterMempool mempool >> ensureHeight hTgt
          False -> return c
  _ <- ensureHeight (Height 1)
  ----------------------------------------------------------------
  -- Generate random transactions and put them into mempool
  forM_ mWK $ \(PrivValidator wallet) -> do
    let myPubKey     = publicKey wallet
        otherWallets = filter (/= myPubKey)
                       [ publicKey w | PrivValidator w <- toList walletKeys]
        chooseWallet = do i <- randomRIO (0, length otherWallets - 1)
                          return (otherWallets !! i)
    cursor <- getMempoolCursor mempool
    forkIO $ forever $ do
      -- Select destination
      output <- chooseWallet
      toSend <- randomRIO (1,100)
      -- Select sources
      (_, CoinState{..}) <- withMVar coinState return
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
            coin <- ensureHeight hBlock
            case foldM (flip processTx) coin txs of
              Nothing -> return False
              Just _  -> return True
          --
        , appBlockGenerator = \hBlock -> do
            coin <- ensureHeight hBlock
            txs  <- peekNTransactions mempool Nothing
            let selectTx _ []     = []
                selectTx c (t:tx) = case processTx t c of
                                      Nothing -> selectTx c  tx
                                      Just c' -> t : selectTx c' tx
            return $ selectTx coin txs
          --
        , appValidator     = mPK
        , appValidatorsSet = validatorSet
        , appMaxHeight     = Just (Height 10)
        }
    , mempool
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
    [ do (appSt,mempool) <- run validatorSet val wallet
         return ( createMockNode net "50000" addr
                , map (,"50000") $ connectRing nodeSet addr
                , appSt
                , mempool
                )
    | (addr,(val,wallet)) <- Map.toList nodeSet
    ]
  --
  s:_ <- runNodeSet nodes
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
