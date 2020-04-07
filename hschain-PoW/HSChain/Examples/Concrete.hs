{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
--import Control.Monad

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

import Data.Char

import qualified Data.List as List

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Word

--import Data.Monoid (Sum(..))
--import Data.Functor.Classes (Show1)
import GHC.Generics (Generic)
import GHC.Read     (Read(..))

import qualified Text.ParserCombinators.ReadP    as ReadP
import qualified Text.ParserCombinators.ReadPrec as Read

import qualified Network.Socket as Net

import Options.Applicative

import HSChain.Crypto
--import HSChain.Crypto.Classes.Hash
--import HSChain.Crypto.SHA
--import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- The tree.

data MerkleTree container alg a =
  Node (Hashed alg a)
       (container (Either
                            (MerkleTree container alg a, MerkleTree container alg a)
                            a
                    ))

-- |Obtain a hash of a node.
getHash :: MerkleTree container alg a -> Hashed alg a
getHash (Node thash _) = thash

instance Eq (Hashed alg a) => Eq (MerkleTree container alg a) where
  a == b = getHash a == getHash b

instance Ord (Hashed alg a) => Ord (MerkleTree container alg a) where
  compare a b = compare (getHash a) (getHash b)

data None a = None
data One a = One a

-- |A tree that knows nothing but the hash of the root.
type MerkleRoot alg a = MerkleTree None alg a

-- |A partially complete tree.
type PartialTree alg a = MerkleTree Maybe alg a

-- |A completely built tree.
type CompleteTree alg a = MerkleTree One alg a

-- |A conversion function from partially built tree into one that is complete.
toCompleteTree :: PartialTree alg a -> Maybe (CompleteTree alg a)
toCompleteTree (Node nhash (Just (Left (l, r)))) =
  (\l' r' -> Node nhash $ One $ Left (l', r')) <$> toCompleteTree l <*> toCompleteTree r
toCompleteTree (Node nhash (Just (Right a))) =
  Just $ Node nhash $ One $ Right $ a
toCompleteTree (Node _hash Nothing) = Nothing

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
toTreeRoot (Node rhash _) = Node rhash None

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
                   (Node phash (One (Left (ln, rn))))
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
    completed = Map.map (\(Node h (Just (Right a))) -> Node h $ One $ Right a) completed'
    updatedTreeDB = treeDB
                      { treeDBPartials = Map.union (treeDBPartials treeDB) realPartials
                      , treeDBOpenPartials = Map.union (treeDBOpenPartials treeDB) open
                      }

-------------------------------------------------------------------------------
-- An interface for transactions.

-- |Transaction must be:
-- - representable in bytestrings,
-- - allow us to create a special mining transaction,
-- - they must provide us with the fee we can spend,
-- - they must answer whether they will play with a set of available UTXOs.
class Serialise tx => Transaction tx where
  -- |A type of UTXO. It may differ between implementations.
  -- One good choice is Hashed alg Something.
  type UTXO tx

  -- |The fee transaction provides.
  transactionFee :: tx -> Integer

  -- |Will transaction play out?
  canTransactionPlay :: Ord (UTXO tx) => Set.Set (UTXO tx) -> tx -> Maybe (Set.Set (UTXO tx))

  -- |Create mining transaction.
  createMiningTransaction :: UTXO tx -> String -> Integer -> Maybe tx


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
-- Configuration.

data Config = Config
              { configOurPort          :: Word16
              , configOtherAddresses   :: [NetAddr]
              }

-------------------------------------------------------------------------------
-- The driver.

main :: IO ()
main = Net.withSocketsDo $ do
  --opts <- 
  return ()
