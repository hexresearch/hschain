{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Full blockchain application
module Thundermint.Blockchain.App where

import Codec.Serialise (Serialise)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
-- import           Data.Foldable
import           Data.Function
import qualified Data.Map        as Map
-- import           Data.Map          (Map)
import Text.Groom

import Thundermint.Blockchain.Types
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Consensus.Algorithm
import Thundermint.Consensus.Types
-- import Thundermint.P2P


----------------------------------------------------------------
--
----------------------------------------------------------------


-- | Main loop for application. Here we update state machine and
--   blockchain in response to incoming messages.
--
--   * INVARIANT: Only this function can write to blockchain
runApplication
  :: (Crypto alg, Serialise a, Show a)
  => AppState alg a
     -- ^ Get initial state of the application
  -> AppChans alg a
     -- ^ Channels for communication with peers
  -> IO ()
runApplication as@AppState{..} ac
  -- NOTE: blockchain state is stored in TVar so there's no need to
  --       thread it explicitly.
  -- FIXME: at the moment we start from genesis block but we need to
  --        pass valid last commit
  = flip fix Nothing $ \loop cm -> do
      cm' <- heightLoop as ac cm
      h   <- readTVarIO appBlockchain >>= \case
        Cons    b _ -> return $ headerHeight $ blockHeader b
        Genesis b   -> return $ headerHeight $ blockHeader b
      case appMaxHeight of
        Just h' | h' > h -> return ()
        _                -> loop (Just cm')

-- Loop where we decide which block we need to commit at given height
heightLoop
  :: (Crypto alg, Serialise a, Show a)
  => AppState alg a
  -> AppChans alg a
  -> Maybe (Commit alg a)
  -> IO (Commit alg a)
heightLoop appSt@AppState{..} appCh@AppChans{..} lastCommt = do
  hParam <- makeHeightParametes appSt appCh
  let totalPower       = sum $ fmap validatorVotingPower appValidatorsSet
      votingPower addr = case Map.lookup addr appValidatorsSet of
        Just i  -> validatorVotingPower i
        Nothing -> 0
      tmState0 = TMState
        { smRound         = Round 0
        , smStep          = StepProposal
        , smPrevotesSet   = emptySignedSetMap votingPower totalPower
        , smPrecommitsSet = emptySignedSetMap votingPower totalPower
        , smProposals     = Map.empty
        , smLockedBlock   = Nothing
        , smLastCommit    = lastCommt
        }
  mtm <- runConsesusM (newHeight hParam tmState0)
  case mtm of
    Success tm -> loop hParam tm
    _          -> error "FIXME: what to do? We cannot fail here!"
  where
    loop hParams tm = do
      appLogger "### Next transition"
      appLogger (groom tm)
      msg <- atomically $ readTChan appChanRx
      appLogger "### Received message"
      appLogger (groom msg)
      let pkLookup a = validatorPubKey <$> Map.lookup a appValidatorsSet
          verify x cont = case verifySignature pkLookup x of
            Just x' -> cont x'
            Nothing -> loop hParams tm
      let recur m =
            runConsesusM (tendermintTransition hParams m tm) >>= \case
              Success tm'    -> loop hParams tm'
              Tranquility    -> loop hParams tm
              Misdeed        -> loop hParams tm
              DoCommit cmt b -> do
                blockStore <- readTVarIO appBlockStore
                case Map.lookup b blockStore of
                  Just blk -> atomically $ modifyTVar appBlockchain (Cons blk) 
                  Nothing  -> error "Panic: no block to commit!"
                return cmt
      case msg of
        RxPreVote   v -> verify v (recur . PreVoteMsg)
        RxPreCommit v -> verify v (recur . PreCommitMsg)
        RxProposal  p -> verify p $ \sp ->
          -- FIXME: we need to separate wheat (GoodProposal) from chaff (InvalidProposal)
          let Proposal{..} = signedValue sp
          in recur $ ProposalMsg propHeight propRound (GoodProposal propBlockID)
        RxTimeout t -> recur (TimeoutMsg t)



----------------------------------------------------------------
-- Concrete implementation of ConsensusMonad
----------------------------------------------------------------

-- | Analog of @ExceptT Err IO@
newtype ConsensusM alg a b = ConsensusM
  { runConsesusM :: IO (ConsensusResult alg a b) }
  deriving (Functor)

data ConsensusResult alg a b
  = Success b
  | Tranquility
  | Misdeed
  | DoCommit  (Commit alg a) (BlockID alg a)
  deriving (Functor)

instance Applicative (ConsensusM alg a) where
  pure  = return
  (<*>) = ap

instance Monad (ConsensusM alg a) where
  return = ConsensusM . return . Success
  ConsensusM m >>= f = ConsensusM $ m >>= \case
    Success a     -> runConsesusM (f a)
    Tranquility   -> return Tranquility
    Misdeed       -> return Misdeed
    DoCommit tm b -> return $ DoCommit tm b

instance MonadIO (ConsensusM alg a) where
  liftIO = ConsensusM . fmap Success

instance ConsensusMonad (ConsensusM alg a) where
  tranquility = ConsensusM $ return Tranquility
  misdeed     = ConsensusM $ return Misdeed
  panic       = error

makeHeightParametes
  :: (Crypto alg, Serialise a)
  => AppState alg a
  -> AppChans alg a
  -> IO (HeightParameres (ConsensusM alg a) alg a)
makeHeightParametes AppState{..} AppChans{..} = do
  h <- readTVarIO appBlockchain >>= \case
    Cons    b _ -> return $ headerHeight $ blockHeader b
    Genesis b   -> return $ headerHeight $ blockHeader b
  return HeightParameres
    { currentH        = h
    --
    , scheduleTimeout = \t -> liftIO $ void $ forkIO $ do
        threadDelay (1*1000*1000)
        atomically $ writeTChan appChanRx $ RxTimeout t
    --
    , castPrevote     = \r b -> liftIO $ do
        let pk   = validatorPrivKey appValidator
            vote = Vote { voteHeight  = h
                        , voteRound   = r
                        , voteTime    = Time 0
                        , voteBlockID = b
                        }
            svote  = signValue pk vote
        atomically $ writeTChan appChanTx (TxPreVote svote)
    --
    , castPrecommit   = \r b -> liftIO $ do
        let pk   = validatorPrivKey appValidator
            vote = Vote { voteHeight  = h
                        , voteRound   = r
                        , voteTime    = Time 0
                        , voteBlockID = b
                        }
            svote  = signValue pk vote
        atomically $ writeTChan appChanTx (TxPreCommit svote)
    --
    , proposeBlock    = \r b -> liftIO $ do
        blockStore <- readTVarIO appBlockStore
        blck       <- case b `Map.lookup` blockStore of
          Just x  -> return x
          Nothing -> error "FIXME: internal error: missing block"
        let pk   = validatorPrivKey appValidator
            prop = Proposal { propHeight    = h
                            , propRound     = r
                            , propTimestamp = Time 0
                            , propPOL       = Nothing
                            , propBlockID   = b
                            , propBlock     = blck
                            }
            sprop  = signValue pk prop
        atomically $ writeTChan appChanTx (TxProposal sprop)
      -- FIXME: What are doing here???
    , makeProposal    = \r cm -> liftIO $ atomically $ do
        p <- appProposalMaker r cm
        modifyTVar appBlockStore $ Map.insert (propBlockID p) (propBlock p)
        return $ propBlockID p

      -- FIXME: this is some random algorithms that should probably
      --        work (for some definition of work)
    , areWeProposers  = \(Round r) ->
        let Height h' = h
            n         = Map.size appValidatorsSet
            i         = (h' + r) `mod` fromIntegral n
            addr      = Map.keys appValidatorsSet !! fromIntegral i
        in addr == address (publicKey (validatorPrivKey appValidator))
    , commitBlock     = \tm b -> ConsensusM $ return $ DoCommit tm b
    }
