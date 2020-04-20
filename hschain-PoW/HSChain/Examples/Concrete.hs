{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
--{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |Concrete.hs
--
-- Concrete implementation of a PoW-based blockchain with UTXO.
--

module HSChain.Examples.Concrete where

import Codec.Serialise

--import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State

import Control.Concurrent
import Control.Concurrent.STM

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

import Data.Char

import Data.Coerce

import qualified Data.List as List

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Time.Clock.POSIX

import Data.Word

--import Data.Monoid (Sum(..))
--import Data.Functor.Classes (Show1)
import GHC.Generics (Generic)
import GHC.Read     (Read(..))

import qualified Text.ParserCombinators.ReadP    as ReadP
import qualified Text.ParserCombinators.ReadPrec as Read

import qualified Network.Socket as Net

import System.IO.Unsafe (unsafePerformIO)

import Options.Applicative

import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.SHA
--import HSChain.Types.Merkle.Types
import HSChain.POW

import ML.NES


----------------------------------------------------------------
-- The tree.

data MerkleTree container alg a =
  Node (Hashed alg (MerkleTree container alg a))
       (container (Either
                            (MerkleTree container alg a, MerkleTree container alg a)
                            a
                    ))
deriving instance Show (container (Either (MerkleTree container alg a, MerkleTree container alg a) a)) => Show (MerkleTree container alg a)

-- |Obtain a hash of a node.
getHash :: MerkleTree container alg a -> Hashed alg (MerkleTree container alg a)
getHash (Node thash _) = thash

instance Eq (Hashed alg a) => Eq (MerkleTree container alg a) where
  a == b = getHash a == getHash b

instance Ord (Hashed alg a) => Ord (MerkleTree container alg a) where
  compare a b = compare (getHash a) (getHash b)

data None a = None deriving Show
data One a = One a deriving Show

-- |A tree that knows nothing but the hash of the root.
type MerkleRoot alg a = MerkleTree None alg a

-- |A partially complete tree.
type PartialTree alg a = MerkleTree Maybe alg a

-- |A completely built tree.
type CompleteTree alg a = MerkleTree One alg a

-- |A conversion function from partially built tree into one that is complete.
toCompleteTree :: PartialTree alg a -> Maybe (CompleteTree alg a)
toCompleteTree (Node nhash (Just (Left (l, r)))) =
  (\l' r' -> Node (coerceHashed nhash) $ One $ Left (l', r')) <$> toCompleteTree l <*> toCompleteTree r
toCompleteTree (Node nhash (Just (Right a))) =
  Just $ Node (coerceHashed nhash) $ One $ Right $ a
toCompleteTree (Node _hash Nothing) = Nothing

-- |Build a complete tree from list of leaves.
completeTreeFromList :: (CryptoHash alg, CryptoHashable a) => [a] -> CompleteTree alg a
completeTreeFromList as = joinLevels $ map toLeaf as
  where
    joinLevels [a] = a
    joinLevels xs = joinLevels $ joinLevel xs
    joinLevel (a:b:xs) =   Node
                              (hash2 a b) (One $ Left (a,b))
                          : joinLevel xs
    joinLevel xs = xs
    toLeaf a = Node (coerceHashed $ hashed a) (One $ Right a)
    hash2 a b = Hashed $ hash $ gethash a <> gethash b
      where
        gethash x = case (getHash x) of { Hashed h -> serialise h }

-------------------------------------------------------------------------------
-- Tree (block) database.

-- |A database of trees.
--
-- There are a map from hashes to complete trees and map of incomplete trees
-- to the hashes of their children.
data TreeDB alg a = TreeDB
  { treeDBCompleted     :: Map (MerkleRoot alg a) (CompleteTree alg a)
    -- ^ A list of completed trees. Only grows.
  , treeDBPartials      :: Map (MerkleRoot alg a) (PartialTree alg a)
    -- ^ A list of partial trees.
  , treeDBOpenPartials  :: Map (MerkleRoot alg a) ()
    -- ^ A list of partials that are still open (have Nothing in the container part).
  }

emptyTreeDB :: TreeDB alg a
emptyTreeDB = TreeDB
  { treeDBCompleted    = Map.empty
  , treeDBPartials     = Map.empty
  , treeDBOpenPartials = Map.empty
  }

-- |Convert to tree root.
toTreeRoot :: MerkleTree container alg a -> MerkleRoot alg a
toTreeRoot (Node rhash _) = Node (coerceHashed rhash) None

-- |Coerce hashes.
coerceHashed :: Hashed alg a -> Hashed alg b
coerceHashed = coerce

-- |Add a set of complete trees into database and propagate completeness
-- upward.
addCompleted :: TreeDB alg a -> Map (MerkleRoot alg a) (CompleteTree alg a) -> TreeDB alg a
addCompleted treeDB newCompleteMap
  | Map.null unseenCompleteMap = treeDB -- nothing had changed.
  | otherwise = addCompleted updatedDB nextCompleted
  where
    unseenCompleteMap = Map.difference newCompleteMap nextCompleted
    updatedDB = treeDB
                  { treeDBCompleted = Map.unions
                                      [ treeDBCompleted treeDB
                                      , newCompleteMap
                                      ]
                  , treeDBPartials  = Map.difference (treeDBPartials treeDB) newCompleteMap
                  , treeDBOpenPartials = Map.difference (treeDBOpenPartials treeDB) newCompleteMap
                  }
    movePartials partial completed = case partial of
        Node phash (Just (Left (l, r))) -- we lift only inner nodes here.
          | Just ln <- Map.lookup (toTreeRoot l) newCompleteMap
          , Just rn <- Map.lookup (toTreeRoot r) newCompleteMap
          -> Map.insert
                   (toTreeRoot partial)
                   (Node (coerceHashed phash) (One (Left (ln, rn))))
                   completed
        _ -> completed
    nextCompleted = Map.foldr movePartials Map.empty $ treeDBPartials treeDB

addPartials :: TreeDB alg a -> [PartialTree alg a] -> TreeDB alg a
addPartials treeDB partials = addCompleted updatedTreeDB completed
  where
    expand this@(Node _ (Just inner)) = Map.insert (toTreeRoot this) this $ case inner of
      Left (l, r) -> Map.union (expand l) (expand r)
      Right _     -> Map.empty
    expand openNode = Map.singleton (toTreeRoot openNode) openNode
    allTrees = Map.unions $ map expand partials
    isOpen (Node _ Nothing) = True
    isOpen _                = False
    (open', notOpen) = Map.partition isOpen allTrees
    open = Map.map (const ()) open'
    isCompleteLeaf (Node _ (Just (Right _))) = True
    isCompleteLeaf _                         = False
    (completed', realPartials) = Map.partition isCompleteLeaf notOpen
    completed = Map.map (\(Node h (Just (Right a))) -> Node (coerceHashed h) $ One $ Right a) completed'
    updatedTreeDB = treeDB
                      { treeDBPartials = Map.union (treeDBPartials treeDB) realPartials
                      , treeDBOpenPartials = Map.union (treeDBOpenPartials treeDB) open
                      }

-------------------------------------------------------------------------------
-- An interface for transactions.

type Height = Int

-- |Transaction must be:
-- - representable in bytestrings,
-- - allow us to create a special mining transaction,
-- - they must provide us with the fee we can spend,
-- - they must answer whether they will play with a set of available UTXOs.
class (Ord (UTXO tx), Serialise tx) => Transaction tx where
  -- |A type of UTXO. It may differ between implementations.
  -- One good choice is Hashed alg Something.
  type UTXO tx

  -- |Initial transaction with all money in the world.
  allMoneyInWorldTx :: String -> tx

  -- |The fee transaction provides.
  transactionFee :: tx -> Integer

  -- |Will transaction play out?
  canTransactionPlay :: Set.Set (UTXO tx) -> tx -> Maybe (Set.Set (UTXO tx))

  -- |Create mining transaction.
  createMiningTransaction :: Height -> UTXO tx -> String -> Integer -> Maybe tx

  -- |Fetch mining reward UTXO.
  tryGetMiningRewardUTXO :: tx -> Maybe (UTXO tx)

-------------------------------------------------------------------------------
-- Mining pool.

data MiningPool alg tx = MiningPool
                       { miningPoolTransactions :: Set tx
                       }

-- |Create a pool.
newMiningPool :: MiningPool alg tx
newMiningPool = MiningPool Set.empty

-- |Add a transaction into a pool.
receiveTransaction :: Ord tx => MiningPool alg tx -> tx -> MiningPool alg tx
receiveTransaction miningPool@MiningPool{..} tx
  = miningPool { miningPoolTransactions = Set.insert tx miningPoolTransactions }

-- |Get transactions for a block.
getTransactionsToMine :: (Transaction tx, Ord tx, Ord (UTXO tx))
                      => Int -> Set (UTXO tx)
                      -> MiningPool alg tx
                      -> ([(ByteString, tx)], MiningPool alg tx)
getTransactionsToMine blockSize startUTXOSet miningPool@MiningPool{..} =
  (goodList, peeledMiningPool)
  where
    peeledMiningPool = miningPool { miningPoolTransactions = remainingTransactions }
    (goodList, remainingTransactions) =
            peelPool blockSize [] startUTXOSet Set.empty miningPoolTransactions
    peelPool availableSize acc utxoSet notPlayed currentTransactions
      | availableSize < 64 = (acc, Set.union notPlayed currentTransactions)
      -- ^ Some threshold to not to go over all transactions.
      | otherwise = case Set.minView currentTransactions of
          Nothing -> (reverse acc, notPlayed)
          Just (tx, remaining)
            | nextAvailableSize < 0 -> skip
            | Just playedUTXOSet <- canTransactionPlay utxoSet tx
              -> peelPool nextAvailableSize ((encoded, tx) : acc) playedUTXOSet notPlayed remaining
            | otherwise -> skip
            where
              encoded = serialise tx
              nextAvailableSize = availableSize - fromIntegral (B.length encoded)
              skip = peelPool availableSize acc utxoSet (Set.insert tx notPlayed) remaining

-------------------------------------------------------------------------------
-- Transactions.

type Noin = Integer
data TX = TX { txInputs, txOutputs :: [(String, Noin)] }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialise)

instance CryptoHashable TX where
  hashStep = hashStep . serialise

nanoNoin, milliNoin, microNoin, noin, kiloNoin, megaNoin, gigaNoin :: Noin
nanoNoin : microNoin : milliNoin : noin : kiloNoin : megaNoin : gigaNoin :_ = iterate (*1000) 1

type Block = CompleteTree SHA256 TX

data BlockHeaderBase = BlockHeaderBase
  { blockHeaderBaseHeight             :: Height
  , blockHeaderBasePrevious           :: Maybe (Hashed SHA256 BlockHeader)
  , blockHeaderBaseSeconds            :: Word64 -- ^Unix time seconds.
  , blockHeaderBaseComplexityShift    :: Word16
  , blockHeaderBaseComplexityMantissa :: Word16
  , blockHeaderBaseBlockHash          :: Hashed SHA256 (CompleteTree SHA256 TX)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialise)

instance CryptoHashable BlockHeaderBase where
  hashStep bhb = hashStep $ serialise bhb

data BlockHeader = BlockHeader
  { blockHeaderBase     :: BlockHeaderBase
  , blockHeaderSolution :: ByteString
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialise)

instance CryptoHashable BlockHeader where
  hashStep (BlockHeader bhb solution) = hashStep $ serialise bhb <> solution

currentSeconds :: IO Word64
currentSeconds = do
  fmap round getPOSIXTime

{-# NOINLINE genesisBlockHeaderBase #-}
{-# NOINLINE genesisBlock #-}
{-# NOINLINE genesisBlockHeader #-}
genesisBlock :: Block
genesisBlockHeaderBase :: BlockHeaderBase
genesisBlockHeader :: BlockHeader
(genesisBlock, genesisBlockHeader, genesisBlockHeaderBase) = unsafePerformIO $ do
  mineGenesis 0
  where
    mineGenesis n = do
      putStrLn $ "genesis attempt "++show n
      seconds <- currentSeconds
      let allMoneyN = allMoneyInWorldTx $ "attempt-"++show n
      let blk = completeTreeFromList [allMoneyN]
      let hbase = BlockHeaderBase
                  { blockHeaderBaseHeight             = 0
                  , blockHeaderBasePrevious           = Nothing
                  , blockHeaderBaseSeconds            = seconds
                  , blockHeaderBaseComplexityShift    = fromIntegral $ powComplexityShift $ powCfgComplexity defaultPOWConfig
                  , blockHeaderBaseComplexityMantissa = powComplexityMantissa $ powCfgComplexity defaultPOWConfig
                  , blockHeaderBaseBlockHash          = getHash blk
                  }
      r <- fmap snd $ tryMineBlock defaultPOWConfig hbase
      case r of
        Just bhdr -> return (blk, bhdr, hbase)
        Nothing -> mineGenesis (n+1)

tryMineBlock :: POWConfig -> BlockHeaderBase -> IO (ByteString, Maybe BlockHeader)
tryMineBlock powConfig hbase = do
  (mh, mbSolutionHash) <- solve [B.toStrict serialisedHeaderBase] powConfig
  return (B.fromStrict mh, fmap addSolution mbSolutionHash)
  where
    addSolution (solution, _) = BlockHeader hbase $ B.fromStrict solution
    serialisedHeaderBase = serialise hbase

miningUTXOPrefix :: String
miningUTXOPrefix = "miningUTXO"

instance Transaction TX where
  type UTXO TX = (String, Noin)

  allMoneyInWorldTx infoAsNonce =
    TX [] [(miningUTXOPrefix++show 0++"-"++infoAsNonce, 100500 * gigaNoin)]

  transactionFee (TX ins outs) = sum (map snd ins) - sum (map snd outs)

  canTransactionPlay utxos (TX ins outs)
    | Set.null unknown = Just $ Set.unions [Set.fromList outs, Set.difference utxos insSet]
    | otherwise = Nothing
    where
      unknown = Set.difference insSet utxos
      insSet = Set.fromList ins

  createMiningTransaction height prevMiningUTXO@(utxoID, noins) infoAsNonce fee
    | noins + fee < 1 = Nothing
    | otherwise = Just $
      TX [prevMiningUTXO] [(miningUTXOPrefix++show height++"-"++infoAsNonce, noins'), ("mining_fee_"++show height, noins + fee - noins)]
    where
      noins' = if noins > noin then noins - noin else noins

  tryGetMiningRewardUTXO (TX _ outs) = case filter hasPrefix outs of
    (utxo:_) -> Just utxo
    _ -> Nothing
    where
      hasPrefix (s, _) = miningUTXOPrefix == take (length miningUTXOPrefix) s

-------------------------------------------------------------------------------
-- Network part.

newUDPSocket :: Word16 -> IO Net.Socket
newUDPSocket ourPort = do
  ai:_ <- Net.getAddrInfo (Just udpHints) Nothing
    (Just $ show ourPort)
  sock <- Net.socket (Net.addrFamily     ai)
                     (Net.addrSocketType ai)
                     (Net.addrProtocol   ai)
  Net.setSocketOption sock Net.ReuseAddr 0
  return sock
  where
    udpHints       = Net.defaultHints
                         { Net.addrFlags      = []
                         , Net.addrSocketType = Net.Datagram
                         }


-- | Network address. It's distinct from 'NetAddr' from @network@
--   package in that it only support IP and could be serialised.
data NetAddr
  = NetAddrV4 !Net.HostAddress  !Word16
  | NetAddrV6 !Net.HostAddress6 !Word16
  deriving stock    (Eq, Ord, Generic)
  deriving anyclass (Serialise)

-- | Convert address from @network@ to serialisable address. Will
--   throw if address other than IP4\/6 is passed to it. But that
--   shouldn't be problem in practice.
sockAddrToNetAddr :: Net.SockAddr -> NetAddr
sockAddrToNetAddr sa = case sa of
  Net.SockAddrInet  port ha     -> NetAddrV4 ha $ fromIntegral port
  Net.SockAddrInet6 port _ ha _ -> NetAddrV6 ha $ fromIntegral port
  _                             -> error $ "unsupported socket address kind: "++show sa

-- | Convert IP address to representation from @network@ library.
netAddrToSockAddr :: NetAddr -> Net.SockAddr
netAddrToSockAddr (NetAddrV4 ha port) = Net.SockAddrInet  (fromIntegral port)   ha
netAddrToSockAddr (NetAddrV6 ha port) = Net.SockAddrInet6 (fromIntegral port) 0 ha 0

instance Show NetAddr where
  show (NetAddrV4 ha p) = let (a,b,c,d) = Net.hostAddressToTuple ha
                       in ((++show p) . (++":")) $ List.intercalate "." $ map show [a,b,c,d]
  show (NetAddrV6 ha p) = let (a,b,c,d,e,f,g,h) = Net.hostAddress6ToTuple ha
                       in ((++show p) . (++".")) $ List.intercalate ":" $ map show [a,b,c,d,e,f,g,h]
instance Read NetAddr where
  readPrec
    = Read.lift $ optional (ReadP.string "tcp://") *> (readV4 <|> readV6)
    where
      readV4
        = NetAddrV4
       <$> (Net.tupleToHostAddress <$>
             ((,,,) <$> digit <* ReadP.char '.'
                    <*> digit <* ReadP.char '.'
                    <*> digit <* ReadP.char '.'
                    <*> digit)
           )
       <*  ReadP.char ':'
       <*> digit
      --
      readV6
        = NetAddrV6
         <$> (Net.tupleToHostAddress6 <$>
             ((,,,,,,,) <$> digit <* ReadP.char ':' <*> digit <* ReadP.char ':'
                        <*> digit <* ReadP.char ':' <*> digit <* ReadP.char ':'
                        <*> digit <* ReadP.char ':' <*> digit <* ReadP.char ':'
                        <*> digit <* ReadP.char ':' <*> digit)
             )
         <*  ReadP.char '.'
         <*> digit
      --
      digit :: Integral i => ReadP.ReadP i
      digit = fromInteger . read <$> ReadP.munch1 isDigit


-------------------------------------------------------------------------------
-- A monad to optimize search parameters.

data OptState = OptState
  { optStateRandoms :: [Double]
  }

newtype OptM a = OptM { runOptM :: StateT OptState IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

deriving instance MonadState OptState OptM

instance Draw OptM where

-------------------------------------------------------------------------------
-- Algorithm.

data DB = DB
  { dbCurrentHead           :: BlockHeader
    -- ^Cannot be non-empty, we have at least genesis.
    -- Must refer to some block in the blocks DB.
  , dbBlocks                :: Map (Hashed SHA256 Block) Block
  , dbChainHeaders          :: Map Height (BlockHeader)
  -- ^More or less good structure - relatively efficient at
  -- cutting maximum elements and addressing with height.
  }
  deriving stock (Show)

startDB :: DB
startDB = DB
  { dbCurrentHead           = genesisBlockHeader
  , dbBlocks                = Map.singleton (getHash genesisBlock) genesisBlock
  , dbChainHeaders          = Map.singleton 0 genesisBlockHeader
  }

miningProcess :: TVar DB -> IO ()
miningProcess dbvar = do
  miningLoop 0
  where
    miningLoop n = do
      db <- atomically $ readTVar dbvar
      (minHash, mbHdrBlock) <- formAndMine n db
      case mbHdrBlock of
        Just (newHeader, newBlock) -> do
          atomically $ do
            db@DB{..} <- readTVar dbvar
            -- here we replace current head, in the real thing we must check work done.
            -- but we can add block unconditionally.
            let newHeight = blockHeaderBaseHeight $ blockHeaderBase newHeader
                db' = db
                      { dbCurrentHead        = newHeader
                      , dbBlocks             = Map.insert (getHash newBlock) newBlock dbBlocks
                      , dbChainHeaders       = Map.insert newHeight newHeader dbChainHeaders
                      }
            writeTVar dbvar db'
          putStrLn $ "mined: "++show newHeader
          miningLoop (n+1)
        Nothing -> miningLoop (n+1)
    extractSeconds = blockHeaderBaseSeconds . blockHeaderBase
    formAndMine n DB{..} = do
      currTime <- currentSeconds
      let adjustModulo = powMainBlocksBetweenAdjustment $ powCfgMain defaultPOWConfig
          headerBefore = Map.findWithDefault
                          (error "no block header at some previous height???")
                          (blockHeaderBaseHeight - adjustModulo)
                          dbChainHeaders
          secondsBetweenHeaders = blockHeaderBaseSeconds - extractSeconds headerBefore
          complexityFP =   fromIntegral (powComplexityMantissa powComplexity)
                         * 2 ** (fromIntegral $ negate $ powComplexityShift powComplexity)
          adjustedComplexityFP = complexityFP * (fromIntegral secondsBetweenHeaders / fromIntegral adjustModulo) / (fromIntegral $ powMainSecondsForBlock (powCfgMain powConfig))
          (newShift, newMantissa) = computeMantissaShift 0 adjustedComplexityFP
          adjustedComplexity
            | False && -- FIXME: WE TURNED OFF COMPLEXITY ADJUSTMENT HERE!!!
                       -- it is needed so we can debug parameter space adjustments.
                 blockHeaderBaseHeight > 0
              && blockHeaderBaseHeight `mod` adjustModulo == 0 =
                   powComplexity
                     { powComplexityShift    = newShift
                     , powComplexityMantissa = newMantissa
                     }
            | otherwise = powComplexity
          powConfig = defaultPOWConfig { powCfgComplexity = adjustedComplexity }
      -- TODO: here we have to access mempool.
      let Just tx = createMiningTransaction nextHeight prevUTXO ("attempt"++show n) 0
          newTxs = [tx]
          newBlock = completeTreeFromList newTxs
          newBlockHeaderBase = BlockHeaderBase
                                 { blockHeaderBaseHeight             = nextHeight
                                 , blockHeaderBasePrevious           = Just $ hashed dbCurrentHead
                                 , blockHeaderBaseSeconds            = currTime
                                 , blockHeaderBaseComplexityShift    = fromIntegral $
                                           powComplexityShift adjustedComplexity
                                 , blockHeaderBaseComplexityMantissa =
                                           powComplexityMantissa adjustedComplexity
                                 , blockHeaderBaseBlockHash          =
                                           getHash newBlock
                                 }
      fmap (\(a,b) -> (a, fmap (flip (,) newBlock) b)) $ tryMineBlock powConfig newBlockHeaderBase
      where
        nextHeight = blockHeaderBaseHeight + 1
        prevMinedBlock = Map.findWithDefault (error "can't find previousblock")
                               blockHeaderBaseBlockHash
                               dbBlocks
        Node _ (One (Right prevMineTx)) = prevMinedBlock
        Just prevUTXO = tryGetMiningRewardUTXO prevMineTx
        BlockHeader{..} = dbCurrentHead
        BlockHeaderBase{..} = blockHeaderBase
        powComplexity = (powCfgComplexity defaultPOWConfig)
                      { powComplexityMantissa = blockHeaderBaseComplexityMantissa
                      , powComplexityShift    = fromIntegral $ blockHeaderBaseComplexityShift
                      }
computeMantissaShift :: Int -> Double -> (Int, Word16)
computeMantissaShift shift x
  | x >= 32768.0 = (shift, round x)
  | otherwise = computeMantissaShift (shift + 1) (x * 2)

node :: Config -> IO ()
node cfg = do
  sock <- newUDPSocket (configOurPort cfg)
  db <- atomically $ newTVar startDB
  forkIO $ miningProcess db
  loop
  where
    loop = do
      threadDelay 1000000
      loop

-------------------------------------------------------------------------------
-- Configuration.

data Config = Config
              { configOurPort          :: Word16
              , configOtherAddresses   :: [NetAddr]
              }

parseConfig :: Parser Config
parseConfig = Config <$> parsePort <*> some parseOtherAddress
  where
    parsePort = option (maybeReader readPort) (long "port" <> short 'p')
    readPort s = case reads s of
      (p, "") : _ -> Just (p :: Word16)
      _ -> Nothing
    parseOtherAddress = option (maybeReader readAddress) (long "peer" <> short 'P')
    readAddress s = case reads s of
      (a,"") : _ -> Just (a :: NetAddr)
      _ -> Nothing

-------------------------------------------------------------------------------
-- The driver.

main :: IO ()
main = Net.withSocketsDo $ do
  config <- execParser (info parseConfig mempty)
  node config
  return ()
