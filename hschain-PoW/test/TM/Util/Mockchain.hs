-- |
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module TM.Util.Mockchain where

import Codec.Serialise
import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail         (MonadFail)
#endif
import Data.Bits
import Data.List  (unfoldr)
import Data.Word
import qualified Data.ByteString as BS
import System.Random    (randoms, mkStdGen)
import System.IO.Unsafe (unsafePerformIO)

import HSChain.Crypto
import HSChain.Control.Class
import HSChain.Examples.Simple
import HSChain.Logger
import HSChain.PoW.Types
import HSChain.PoW.Consensus
import HSChain.Store.Query
import HSChain.Types.Merkle.Types
import HSChain.Examples.Coin

----------------------------------------------------------------
--
----------------------------------------------------------------

mockchain :: (Num (Nonce cfg), Show (Nonce cfg), KVConfig cfg)
          => [Block (KV cfg)]
mockchain = gen : unfoldr ( Just
                          . (\b -> (b,b))
                          . (\b -> mineBlock [let Height h = blockHeight b in (fromIntegral h, "VAL")] b)
                          ) gen
  where
    gen = GBlock { blockHeight = Height 0
                 , blockTime   = Time 0
                 , prevBlock   = Nothing
                 , blockData   = KV { kvData       = merkled []
                                    , kvNonce      = 0
                                    , kvTarget     = Target $ shiftL 1 256 - 1
                                    }
                 }


emptyCoinChain :: [Block Coin]
emptyCoinChain = gen : unfoldr (Just . (\b -> (b,b)) . mineCoin [] . Just) gen
  where
    gen = mineCoin [] Nothing

mineCoin :: [TxCoin] -> Maybe (Block Coin) -> Block Coin
mineCoin txs mb = GBlock
  { blockHeight = maybe (Height 0) (succ . blockHeight) mb
  , blockTime   = Time 0
  , prevBlock   = blockID <$> mb
  , blockData   = Coin { coinData       = merkled txs
                       , coinNonce      = 1337
                       , coinTarget     = Target $ shiftL 1 256 - 1
                       }
  }

mineBlock :: (Num (Nonce cfg), Show (Nonce cfg), KVConfig cfg)
          => [(Int,String)] -> Block (KV cfg) -> Block (KV cfg)
mineBlock txs b = unsafePerformIO $ do
  find $ GBlock
    { blockHeight = succ $ blockHeight b
    , blockTime   = Time 0
    , prevBlock   = Just $! blockID b
    , blockData   = KV { kvData   = merkled txs
                       , kvNonce  = 0
                       , kvTarget = kvTarget (blockData b)
                       }
    }
  where
    find blk = do
      r <- fst <$> adjustPuzzle blk
      case r of
        Just b' -> return b'
        Nothing -> let Time t = blockTime blk
                   in find (blk { blockTime = Time (t+1)})

data MockChain

instance Serialise (Nonce MockChain) => KVConfig MockChain where
  type Nonce MockChain = Word64
  kvDefaultNonce   = const 0
  kvAdjustInterval = Const 100
  kvBlockTimeInterval  = Const (Time 1000)
  kvSolvePuzzle blk = case solved of
    blk' : _ -> return (Just blk')
    _ -> return Nothing
    where
      nonces = map (+ kvNonce (blockData blk)) [0..2^(24 :: Int) - 1]
      solved = [ blk'
               | nonce <- nonces
               , let blk' = blk { blockData = (blockData blk) { kvNonce = nonce } }
               , let hdr' = toHeader blk'
               , let tgt' = hash256AsTarget hdr'
               , tgt' <= tgt
               ]
      tgt = blockTargetThreshold blk
  kvCheckPuzzle hdr = return $ blockTargetThreshold hdr >= resultTgt
    where
      resultTgt = hash256AsTarget hdr

genesis, block1, block2, block3, block2', block3', block4' :: Block (KV MockChain)
genesis: block1: block2: block3:_ = mockchain
block2' = mineBlock [(2,"Z")] block1
block3' = mineBlock [(3,"Z")] block2'
block4' = mineBlock [(4,"Z")] block3'


header1, header2, header3, header2', header3', header4' :: Header (KV MockChain)
header1  = toHeader block1
header2  = toHeader block2
header3  = toHeader block3
header2' = toHeader block2'
header3' = toHeader block3'
header4' = toHeader block4'


-- | State view which doesn't do any block validation whatsoever
inMemoryView
  :: (Monad m, BlockData b)
  => BlockID b
  -> StateView m b
inMemoryView = make (error "No revinding past genesis")
  where
    make previous bid = view
      where
        view = StateView
          { stateBID    = bid
          , applyBlock  = \_ bh _ -> return $ Right $ make view (bhBID bh)
          , revertBlock = return previous
          , flushState  = return view
          , checkTx                  = error "Transaction checking is not supported"
          , createCandidateBlockData = error "Block creation is not supported"
          }

-- | Monad transformer for use in tests
newtype HSChainT m x = HSChainT (ReaderT (Connection 'RW) m x)
  deriving newtype (Functor,Applicative,Monad,MonadIO,MonadFail)
  deriving newtype (MonadThrow,MonadCatch,MonadMask,MonadFork)
  deriving newtype (MonadReader (Connection 'RW))
  -- HSChain instances
  deriving MonadLogger            via NoLogsT          (HSChainT m)
  deriving (MonadReadDB, MonadDB) via DatabaseByReader (HSChainT m)

runHSChainT :: Connection 'RW -> HSChainT m x -> m x
runHSChainT c (HSChainT m) = do
  runReaderT m c

withHSChainT :: (MonadIO m, MonadMask m) => HSChainT m a -> m a
withHSChainT m = withConnection "" $ \c -> runHSChainT c m

withHSChainTDB :: (MonadIO m, MonadMask m) => FilePath -> HSChainT m a -> m a
withHSChainTDB db m = withConnection db $ \c -> runHSChainT c m

data Abort = Abort Height
  deriving stock    (Show)
  deriving anyclass (Exception)

catchAbort :: MonadCatch m => (forall a. m a) -> m Height
catchAbort action = handle (\(Abort h) -> return h) action

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
