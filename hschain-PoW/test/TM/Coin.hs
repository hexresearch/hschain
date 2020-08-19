{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
-- |
module TM.Coin (tests) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import qualified Data.ByteString as BS
import System.Random (randoms, mkStdGen)
import Data.Coerce
import Data.List (unfoldr,sort)
import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit
import System.Directory (removePathForcibly)
import GHC.Generics (Generic)

import HSChain.Crypto
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.PoW.BlockIndex
import HSChain.Examples.Coin
import HSChain.Types.Merkle.Types
import HSChain.Store.Query
import TM.Util.Mockchain (HSChainT, withHSChainT, withHSChainTDB, emptyCoinChain, mineCoin)

----------------------------------------------------------------
--
----------------------------------------------------------------

coinInit :: IO ()
coinInit = withHSChainT $ do
  _ <- coinStateView $ head emptyCoinChain
  return ()

-- Simple application of empty blocks
coinTrivialFwd :: Bool -> IO ()
coinTrivialFwd doFlush = withHSChainT $ do
  (db,_,st0) <- coinStateView g
  mapM_ (storeBlock db) [b1, b2]
  -- Block 1
  expectFail "1-1" st0 bh2 b1   -- BH mismatch 1
  expectFail "1-2" st0 bh1 b2   -- BH mismatch 2
  expectFail "1-3" st0 bh2 b2   -- Unrelated block
  st1 <- flush =<< expectOK "B1" st0 bh1 b1
  liftIO $ bhBID bh1 @=? stateBID st1
  -- Block 2
  expectFail "2-1" st1 bh2 b1   -- BH mismatch 1
  expectFail "2-2" st1 bh1 b2   -- BH mismatch 2
  expectFail "2-3" st1 bh1 b1   -- Unrelated block 
  st2 <- expectOK "B2" st1 bh2 b2
  liftIO $ bhBID bh2 @=? stateBID st2
  where
    flush | doFlush   = flushState
          | otherwise = return
    g:b1:b2:_ = emptyCoinChain
    -- Build block index
    bIdx = addBlock b2
         $ addBlock b1
         $ blockIndexFromGenesis g
    Just bh1 = lookupIdx (blockID b1) bIdx
    Just bh2 = lookupIdx (blockID b2) bIdx
    --
    expectOK msg s bh b = applyBlock s bIdx bh b >>= \case
      Right s' -> return s'
      Left  e  -> liftIO $ assertFailure $ msg ++ ": " ++ show e
    --
    expectFail msg s bh b = applyBlock s bIdx bh b >>= \case
      Right _  -> liftIO $ assertFailure $ msg ++ ": unxpected success"
      Left  _  -> return ()


-- Simple application of empty blocks
coinTrivialRollback :: Bool -> IO ()
coinTrivialRollback doFlush = withHSChainT $ do
  (db,_,st0) <- coinStateView g
  mapM_ (storeBlock db) [b1, b2]
  -- Block 1
  st1 <- flush =<< expectOK "B1" st0 bh1 b1
  liftIO $ bhBID bh1 @=? stateBID st1
  -- Rollback
  st0' <- revertBlock st1
  liftIO $ stateBID st0 @=? stateBID st0'
  -- Apply block again
  st1' <- flush =<< expectOK "B1" st0' bh1 b1
  liftIO $ bhBID bh1 @=? stateBID st1'
  where
    flush | doFlush   = flushState
          | otherwise = return
    g:b1:b2:_ = emptyCoinChain
    -- Build block index
    bIdx = addBlock b2
         $ addBlock b1
         $ blockIndexFromGenesis g
    Just bh1 = lookupIdx (blockID b1) bIdx
    Just bh2 = lookupIdx (blockID b2) bIdx
    --
    expectOK msg s bh b = applyBlock s bIdx bh b >>= \case
      Right s' -> return s'
      Left  e  -> liftIO $ assertFailure $ msg ++ ": " ++ show e
    --
    expectFail msg s bh b = applyBlock s bIdx bh b >>= \case
      Right _  -> liftIO $ assertFailure $ msg ++ ": unxpected success"
      Left  _  -> return ()

addBlock
  :: (BlockData b, MerkleMap b)
  => Block b -> BlockIndex b -> BlockIndex b
addBlock b bIdx = insertIdx bh bIdx
  where
    Just parent = do bid <- prevBlock b
                     lookupIdx bid bIdx
    bh = BH { bhHeight   = blockHeight b
            , bhTime     = blockTime   b
            , bhBID      = blockID     b
            , bhWork     = blockWork   b
            , bhPrevious = Just parent
            , bhData     = blockData $ toHeader b
            }


coinAddBlock :: IO ()
coinAddBlock = do
  removePathForcibly "db"
  withHSChainTDB "db" $ do
    (db,_,st0) <- coinStateView gen
    mapM_ (storeBlock db) [blk1, blk2]
    --
    st1 <- flush =<< applyBlock st0 bIndex bh1' blk1
    st2 <- flush =<< applyBlock st1 bIndex bh2' blk2
    return ()
  where
    flush (Right s) = flushState s
    flush (Left  e) = error $ show e


----------------------------------------------------------------
-- Mock blockchain
----------------------------------------------------------------

k1,k2 :: PrivKey Alg
k1:k2:_ = makePrivKeyStream 1334

makePrivKeyStream :: forall alg. CryptoSign alg => Int -> [PrivKey alg]
makePrivKeyStream seed
  = unfoldr step
  $ randoms (mkStdGen seed)
  where
    -- Size of key
    keySize = privKeySize (Proxy @alg)
    -- Generate single key
    step stream = Just (k, stream')
      where
        Just k    = decodeFromBS $ BS.pack bs
        (bs, stream') = splitAt keySize stream


----------------------------------------------------------------
--
----------------------------------------------------------------

gen :: Block Coin
gen = mineCoin [] Nothing

blk1,blk2 :: Block Coin
blk1 = mineWithCoinbase k1 gen  []
blk2 = mineWithCoinbase k2 blk1
  [ signTX k1 $ TxSend
      { txInputs  = [UTXO 0 (hashed $ head $ merkleValue $ coinData $ blockData blk1)]
      , txOutputs = [ Unspent (publicKey k1) 20
                    , Unspent (publicKey k2) 80
                    ]
      }
  ]



mineWithCoinbase :: PrivKey Alg -> Block Coin -> [TxCoin] -> Block Coin
mineWithCoinbase k b txs = mineCoin
  ( signTX k (TxSend
      { txInputs  = [ UTXO 0 (coerce (blockID b)) ]
      , txOutputs = [ Unspent (publicKey k) 100    ]
      })
  : txs
  ) (Just b)

bIndex
  = addBlock blk2
  $ addBlock blk1
  $ blockIndexFromGenesis gen
Just bh1' = lookupIdx (blockID blk1) bIndex
Just bh2' = lookupIdx (blockID blk2) bIndex


signTX :: PrivKey Alg -> TxSend -> TxCoin
signTX pk tx = TxCoin (publicKey pk) (signHashed pk tx) tx



----------------------------------------------------------------
-- Framework for chaining updates of block
----------------------------------------------------------------

class (MonadFail m, MonadIO m) => TestMonad m where
  mine     :: PrivKey Alg -> BlockID Coin -> [TxCoin] -> m (Either (BlockException Coin) (BlockID Coin, TxCoin))
  flush    :: m ()
  revert   :: m ()
  liveUTXO :: m [UTXO]


data TestEnv = TestEnv
  { _envState :: StateView (HSChainT IO) Coin
  , _envDB    :: BlockDB   (HSChainT IO) Coin
  , _envBIdx  :: BlockIndex Coin
  }
  deriving Generic

$(makeLenses ''TestEnv)

newtype Test a = Test (StateT TestEnv (HSChainT IO) a)
  deriving newtype ( Functor, Applicative, Monad, MonadIO, MonadFail
                   , MonadState TestEnv
                   )


instance TestMonad Test where
  mine pk bid txs = Test $ do
    -- Create block and put it into store
    db     <- use envDB
    Just b <- lift $ retrieveBlock db bid
    let b'   = mineWithCoinbase pk b txs
        bid' = blockID b'
    lift $ storeBlock db b'
    envBIdx %= addBlock b'
    -- Try to apply block state
    st      <- use envState
    bIdx    <- use envBIdx
    let Just bh = lookupIdx bid' bIdx
    r  <- lift $ applyBlock st bIdx bh b'
    case r of
      Left e    -> return (Left e)
      Right st' -> do envState .= st'
                      return $ Right ( bid'
                                     , head $ merkleValue $ coinData $ blockData b')
  revert = Test $ do
    assign envState =<< lift . revertBlock =<< use envState
  flush = Test $
    assign envState =<< lift . flushState =<< use envState
  liveUTXO = Test $ queryRO $ basicQuery_
    "SELECT n_out, tx_hash FROM coin_utxo JOIN coin_state ON live_utxo = utxo_id"
    

expectUTXO :: TestMonad m => String -> [UTXO] -> m ()
expectUTXO msg utxos = do
  live <- liveUTXO
  liftIO $ assertEqual msg (sort utxos) (sort live)
-- utxoCB :: 

runTest :: Test a -> IO a
runTest (Test m) = withHSChainT $ do
  (_envDB,_,_envState) <- coinStateView gen
  let _envBIdx = blockIndexFromGenesis gen
  evalStateT m TestEnv{..}


testFresh :: TestMonad m => m ()
testFresh = do
  outs <- liveUTXO 
  liftIO $ [] @=? outs

test1B :: TestMonad m => m ()
test1B = do
  Right (_,cb1) <- mine k1 (blockID gen) []
  flush
  expectUTXO "B1" [UTXO 0 (hashed cb1)]

test2B :: TestMonad m => m ()
test2B = do
  Right (bid1,cb1) <- mine k1 (blockID gen) []
  flush
  expectUTXO "B1" [UTXO 0 (hashed cb1)]
  --
  Right (_,cb2) <- mine k2 bid1 []
  flush
  expectUTXO "B2" [ UTXO 0 (hashed cb1)
                  , UTXO 0 (hashed cb2)
                  ]
  --
  revert
  flush
  expectUTXO "R1" [UTXO 0 (hashed cb1)]

test2BNoF :: TestMonad m => m ()
test2BNoF = do
  Right (bid1,cb1) <- mine k1 (blockID gen) []
  Right _          <- mine k2 bid1 []
  revert
  flush
  expectUTXO "B1" [UTXO 0 (hashed cb1)]

testSpend :: TestMonad m => m ()
testSpend = do
  Right (bid1,cb1) <- mine k1 (blockID gen) []
  flush
  -- Spend 1 output
  let tx1 = signTX k1 $ TxSend
        { txInputs  = [UTXO 0 (hashed cb1)]
        , txOutputs = [ Unspent (publicKey k1) 20
                      , Unspent (publicKey k2) 80
                      ]
        }
      txHash1 = hashed tx1
      u1_0 = UTXO 0 txHash1
      u1_1 = UTXO 1 txHash1
  Right (_,cb2) <- mine k2 bid1 [tx1]
  flush
  expectUTXO "" [ UTXO 0 (hashed cb2)
                , u1_0
                , u1_1
                ]
  -- Revert block
  revert
  flush
  expectUTXO "" [ UTXO 0 (hashed cb1) ]
  

----------------------------------------------------------------
--
----------------------------------------------------------------

tests :: TestTree
tests = testGroup "coin"
  [ testCase "init"                 $ coinInit
  , testCase "empty-apply"          $ coinTrivialFwd False
  , testCase "empty-apply-flush"    $ coinTrivialFwd True
  , testCase "empty-rollback"       $ coinTrivialRollback False
  , testCase "empty-rollback-flush" $ coinTrivialRollback True
  , testCase "coinbase"             $ coinAddBlock
  , testGroup "state"
    [ testCase "empty" $ runTest testFresh
    , testCase "1B"    $ runTest test1B
    , testCase "2B"    $ runTest test2B
    , testCase "2BNoF" $ runTest test2BNoF
    , testCase "spend" $ runTest testSpend
    ]
  ]
