{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module TM.Coin (tests) where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import System.Random (randoms, mkStdGen)
import Data.Coerce
import Data.List (unfoldr)
import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit
import System.Directory (removePathForcibly)

import HSChain.Crypto
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.PoW.BlockIndex
import HSChain.Examples.Coin
import HSChain.Types.Merkle.Types
import TM.Util.Mockchain (withHSChainT, withHSChainTDB, emptyCoinChain, mineCoin)

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
  ]

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
  putStrLn ""
  print ("G  =", blockID gen)
  print ("B1 =", blockID blk1)
  print ("B2 =", blockID blk2)
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
blk1 = mineCoin
  [ signTX k1 $ TxSend
      { txInputs  = [ UTXO 0 (coerce (blockID gen)) ]
      , txOutputs = [ Unspent (publicKey k1) 100    ]
      }
  ]
  (Just gen)
blk2 = mineCoin
  [ signTX k2 $ TxSend
      { txInputs  = [ UTXO 0 (coerce (blockID blk1)) ]
      , txOutputs = [ Unspent (publicKey k2) 100    ]
      }
  ]
  (Just blk1)

bIndex
  = addBlock blk2
  $ addBlock blk1
  $ blockIndexFromGenesis gen
Just bh1' = lookupIdx (blockID blk1) bIndex
Just bh2' = lookupIdx (blockID blk2) bIndex


signTX :: PrivKey Alg -> TxSend -> TxCoin
signTX pk tx = TxCoin (publicKey pk) (signHashed pk tx) tx
