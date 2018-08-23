{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Helper function for running mock network of thundermint nodes
module Thundermint.Mock (
    -- * Validators
    makePrivateValidators
  , makeValidatorSetFromPriv
    -- * Network connectivity
  , connectAll2All
  , connectRing
    -- * New node code
  , Abort(..)
  , Topology(..)
  , NodeDescription(..)
  , runNode
  , newBlockStorage
    -- * Running nodes
  , startNode
  , runNodeSet
  ) where

import Codec.Serialise          (Serialise)
import Control.Concurrent.Async hiding (runConcurrently)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Foldable
import Data.Maybe               (isJust)
import Data.Map                 (Map)
import Data.Word                (Word64)
import qualified Data.Aeson             as JSON
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString.Char8  as BC8
import qualified Data.Map               as Map
import qualified Katip
import System.Directory (createDirectoryIfMissing)
import System.FilePath  ((</>),splitFileName)
import System.Random    (randomIO)
import Text.Printf
import GHC.Generics     (Generic)

import Thundermint.Control (MonadFork,runConcurrently)
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Crypto.Ed25519   (Ed25519_SHA512, privateKey)
import Thundermint.Consensus.Types
import Thundermint.Blockchain.App
import Thundermint.Blockchain.Interpretation
import Thundermint.Blockchain.Types
import Thundermint.Logger
import Thundermint.P2P
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.STM
import Thundermint.Store.SQLite
import Thundermint.Debug.Trace



----------------------------------------------------------------
--
----------------------------------------------------------------

data Abort = Abort
  deriving Show
instance Exception Abort

-- | Generate list of private validators
makePrivateValidators
  :: [BS.ByteString]
  -> Map (Address Ed25519_SHA512) (PrivValidator Ed25519_SHA512)
makePrivateValidators keys = Map.fromList
  [ (address (publicKey pk) , PrivValidator pk)
  | bs <- keys
  , let pk = case Base58.decodeBase58 Base58.bitcoinAlphabet bs of
          Just x  -> privateKey x
          Nothing -> error "Incorrect Base58 encoding for bs"
  ]

-- | Create set of all known public validators from set of private
--   validators
makeValidatorSetFromPriv
  :: (Foldable f, Crypto alg)
  => f (PrivValidator alg) -> ValidatorSet alg
makeValidatorSetFromPriv vals =
  case r of
    Right x -> x
    Left  e -> error $ "Dublicate public key in validator: " ++ show e
  where
    r = makeValidatorSet
      [ Validator { validatorPubKey      = publicKey (validatorPrivKey v)
                  , validatorVotingPower = 1
                  }
      | v <- toList vals
      ]




----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Calculate set of addresses for node to connect to
--   assuming all nodes are connected to each other.
connectAll2All :: Ord addr => Map addr a -> addr -> [addr]
connectAll2All vals addr =
  [ a
  | a <- Map.keys vals
  , a < addr
  ]

-- | Connect nodes in ring topology
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

defCfg :: Configuration
defCfg = Configuration
  { timeoutNewHeight   = (500, 500)
  , timeoutProposal    = (500, 500)
  , timeoutPrevote     = (500, 500)
  , timeoutPrecommit   = (500, 500)
  , gossipDelayVotes   = 25
  , gossipDelayBlocks  = 25
  , gossipDelayMempool = 25
  }


data Topology = All2All
              | Ring
              deriving (Generic,Show)
instance JSON.ToJSON   Topology
instance JSON.FromJSON Topology

-- | Specification of node
data NodeDescription addr m alg st tx a = NodeDescription
  { nodeStorage         :: BlockStorage 'RW m alg a
    -- ^ Storage API for nodes
  , nodeBlockChainLogic :: BlockFold st tx a
    -- ^ Storage for blocks
  , nodeNetworks        :: NetworkAPI addr
    -- ^ Network API
  , nodeAddr            :: addr
    -- ^ Node address
  , nodeInitialPeers    :: [addr]
    -- ^ Initial peers
  , nodeValidationKey   :: Maybe (PrivValidator alg)
  , nodeAction          :: Maybe ((tx -> m ()) -> st -> m ())
  , nodeCommitCallback  :: Height -> m ()
  }

-- | Create block storage. It will use SQLite if path is specified or
--   STM otherwise.
newBlockStorage
  :: (Crypto alg, Serialise a, Serialise (PublicKey alg))
  => FilePath                   -- ^ Prefix
  -> Maybe FilePath             -- ^ Path to database
  -> Block alg a
  -> ValidatorSet alg
  -> IO (BlockStorage 'RW IO alg a)
newBlockStorage prefix mpath genesis validatorSet = do
  let makedir path = let (dir,_) = splitFileName path
                     in createDirectoryIfMissing True dir
  case mpath of
      Nothing -> newSTMBlockStorage genesis validatorSet
      Just nm -> do
        let dbName = prefix </> nm
        makedir dbName
        newSQLiteBlockStorage dbName genesis validatorSet


runNode
  :: ( MonadIO m, MonadMask m, MonadFork m, MonadLogger m, MonadTrace m
     , Ord tx, Serialise tx
     , Crypto alg, Ord addr, Show addr, Serialise addr, Show a, LogBlock a
     , Serialise a)
  => NodeDescription addr m alg st tx a
  -> m ()
runNode NodeDescription{nodeBlockChainLogic=logic@BlockFold{..}, ..} = do
  -- Create proposal storage
  propSt      <- newSTMPropStorage
  -- Create state of blockchain & Update it to current state of
  -- blockchain
  hChain      <- blockchainHeight     nodeStorage
  Just valSet <- retrieveValidatorSet nodeStorage (next hChain)
  bchState    <- newBChState logic (makeReadOnly nodeStorage)
  _           <- stateAtH bchState (next hChain)
  -- Create mempool
  let checkTx tx = do
        st <- currentState bchState
        return $ isJust $ processTx (Height 1) tx st
  mempool <- newMempool checkTx
  cursor  <- getMempoolCursor mempool
  -- Build application state of consensus algorithm
  let appSt = AppState
        { appStorage     = nodeStorage
        , appPropStorage = propSt
          --
        , appValidationFun = \h a -> do
            st <- stateAtH bchState h
            return $ isJust $ processBlock h a st
          --
        , appBlockGenerator = \h -> do
            st  <- stateAtH bchState h
            txs <- peekNTransactions mempool Nothing
            return $ transactionsToBlock h st txs
          --
        , appCommitCallback = \h -> setNamespace "mempool" $ do
            before <- mempoolStats mempool
            logger InfoS "Mempool before filtering" before
            filterMempool mempool
            after  <- mempoolStats mempool
            logger InfoS "Mempool after filtering" after
            nodeCommitCallback h
          --
        , appValidator      = nodeValidationKey
        , appValidatorsSet  = valSet
        }
  -- Networking
  appCh <- liftIO newAppChans
  runConcurrently $
    case nodeAction of
        Nothing     -> []
        Just action -> [forever $ action (pushTransaction cursor)
                              =<< currentState bchState]
    ++
    [ id $ setNamespace "net"
         $ startPeerDispatcher defCfg nodeNetworks nodeAddr nodeInitialPeers appCh
                               (makeReadOnly   nodeStorage)
                               (makeReadOnlyPS propSt)
                               mempool
    , id $ setNamespace "consensus"
         $ runApplication defCfg appSt appCh
    ]



----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Start node which will now run consensus algorithm
startNode
  :: (Ord addr, Show addr, Serialise addr, Crypto alg, Serialise a, Serialise tx, Show a, LogBlock a)
  => NetworkAPI addr
  -> addr
  -> [addr]
  -> AppState IO alg a
  -> Mempool IO tx
  -> IO ()
startNode net addr addrs appState@AppState{..} mempool = do
  -- Initialize logging
  logfile <- case appValidator of
    Just (PrivValidator pk) ->
      let Address nm = address $ publicKey pk
      in return $ "val-" ++ BC8.unpack (Base58.encodeBase58 Base58.bitcoinAlphabet nm)
    Nothing -> do
      w1 <- randomIO
      w2 <- randomIO
      return $ printf "node-%016x-%016x" (w1 :: Word64) (w2 :: Word64)
  scribe <- Katip.mkFileScribe ("logs/" ++ logfile) Katip.DebugS Katip.V2
  logenv <- Katip.registerScribe "log" scribe Katip.defaultScribeSettings
        =<< Katip.initLogEnv "TM" "DEV"
  flip finally (Katip.closeScribes logenv) $ do
    appCh   <- newAppChans
    let netRoutine = runLoggerT "net" logenv
                   $ startPeerDispatcher defCfg net addr addrs appCh
                       (hoistBlockStorageRO liftIO $ makeReadOnly   appStorage)
                       (hoistPropStorageRO  liftIO $ makeReadOnlyPS appPropStorage)
                       (hoistMempool liftIO mempool)
    withAsync netRoutine $ \_ ->
      runLoggerT "consensus" logenv
        $ runApplication defCfg (hoistAppState liftIO appState) appCh

-- | Start set of nodes and return their corresponding storage. Will
--   return their storage after all nodes finish execution
runNodeSet
  :: (Ord addr, Show addr, Serialise addr, Crypto alg, Serialise a, Show a, Serialise tx, LogBlock a)
  => [( NetworkAPI addr, addr, [addr], AppState IO alg a, Mempool IO tx)]
  -> IO [BlockStorage 'RO IO alg a]
runNodeSet nodes = do
  withAsyncs [ startNode net addr addrs appSt mp
             | (net,addr,addrs,appSt,mp) <- nodes
             ]
    $ void . waitAny
  return [ makeReadOnly $ appStorage a | (_,_,_,a,_) <- nodes ]

withAsyncs :: [IO a] -> ([Async a] -> IO b) -> IO b
withAsyncs ios function
  = recur ([],ios)
  where
    recur (as,[])   = function (reverse as)
    recur (as,i:is) = withAsync i $ \a -> recur (a:as, is)
