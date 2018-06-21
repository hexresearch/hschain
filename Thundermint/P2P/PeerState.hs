{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
-- |
module Thundermint.P2P.PeerState (
    FullStep(..)
    -- * State of peer
  , PeerState(..)
  , LaggingPeer(..)
  , CurrentPeer(..)
    -- * Mutable reference for peer state
  , PeerStateObj
  , newPeerStateObj
  , getPeerState
    -- * Updating state of peer
  , advanceOurHeight
  , advancePeer
  , addProposal
  , addPrevote
  , addPrevoteI
  , addPrecommit
  , addPrecommitI
  , addBlock
  ) where

import Codec.Serialise (Serialise)
import Control.Concurrent hiding (modifyMVar_)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Data.Map as Map
import           Data.Map   (Map)
import qualified Data.Set as Set
import           Data.Set   (Set)

import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Store

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Our knowledge of peer state
data PeerState alg a
  = Lagging !(LaggingPeer alg a)  -- ^ Peer is lagging behind us
  | Current !(CurrentPeer alg a)  -- ^ Peer is at the same height as us
  | Ahead   !FullStep             -- ^ Peer is ahead of us
  | Unknown                       -- ^ We don't know anything about peer yet

-- | State of peer which is lagging behind us. In this case we only
--   interested in precommits which are part of commit justifying next
--   block and whether it have proposal for commit round and block for
--   that round.
data LaggingPeer alg a = LaggingPeer
  { lagPeerStep        :: !FullStep              -- ^ Step of peer
  , lagPeerCommitR     :: !Round                 -- ^ Round when block was commited
  , lagPeerValidators  :: !(ValidatorSet alg)    -- ^ Set of validators for peer's height
  , lagPeerPrecommits  :: !ValidatorISet         -- ^ Precommits that peer have
  , lagPeerHasProposal :: !Bool                  -- ^ Whether peer have proposal
  , lagPeerBlocks      :: !(Set (BlockID alg a)) -- ^ Whether peer have block
  }

-- | Peer which is at the same height as we. Here state is more
--   complicated and somewhat redundant. Tendermint only tracks votes
--   for peer's round. For algorithm simplicity we track
--
--   FIXME: simplify state along tern
data CurrentPeer alg a = CurrentPeer
  { peerStep       :: !FullStep                  -- ^ Step of peer
  , peerValidators :: !(ValidatorSet alg)        -- ^
  , peerPrevotes   :: !(Map Round ValidatorISet) -- ^ Peer's prevotes
  , peerPrecommits :: !(Map Round ValidatorISet) -- ^ Peer's precommits
  , peerProposals  :: !(Set Round)               -- ^ Set of proposals peer has
  , peerBlocks     :: !(Set (BlockID alg a))     -- ^ Set of blocks for proposals
  }

getPeerStep :: PeerState alg a -> Maybe FullStep
getPeerStep = \case
  Lagging p -> Just $ lagPeerStep p
  Current p -> Just $ peerStep    p
  Ahead   s -> Just s
  Unknown   -> Nothing

getPeerHeight :: PeerState alg a -> Maybe Height
getPeerHeight p = do FullStep h _ _ <- getPeerStep p
                     return h


----------------------------------------------------------------

-- | Mutable reference to state of peer.It's safe to mutate
--   concurrently
data PeerStateObj m alg a = PeerStateObj
  (BlockStorage 'RO m alg a)
  (MVar (PeerState alg a))


-- | Create new peer state with default state
newPeerStateObj :: MonadIO m => BlockStorage 'RO m alg a -> m (PeerStateObj m alg a)
newPeerStateObj storage = do
  var <- liftIO $ newMVar Unknown
  return $ PeerStateObj storage var

-- | Obtain current state of peer
getPeerState :: (MonadMask m, MonadIO m)
             => PeerStateObj m alg a -> m (PeerState alg a)
getPeerState (PeerStateObj _ var)
  = withMVarM var return

----------------------------------------------------------------

-- | Local state increased to height H. Update our opinion about
--   peer's state accordingly
advanceOurHeight :: (MonadIO m, MonadMask m)
                 => PeerStateObj m alg a -> Height -> m ()
advanceOurHeight (PeerStateObj BlockStorage{..} var) ourH =
  modifyMVar_ var $ \peer -> case peer of
    Lagging _ -> return peer
    Current p
      | FullStep h _ _ <- peerStep p
      , h /= ourH
        -> do Just vals <- retrieveValidatorSet h
              Just r    <- retrieveCommitRound  h
              return $ Lagging LaggingPeer
                { lagPeerStep        = FullStep h r StepNewHeight
                , lagPeerCommitR     = r
                , lagPeerValidators  = vals
                , lagPeerPrecommits  = emptyValidatorISet $ validatorSetSize vals
                , lagPeerHasProposal = False
                , lagPeerBlocks      = Set.empty
                }
      | otherwise
        -> return peer
    Ahead step@(FullStep h _ _)
      | h == ourH
        -> do Just vals <- retrieveValidatorSet h
              return $ Current CurrentPeer
                { peerStep       = step
                , peerValidators = vals
                , peerPrevotes   = Map.empty
                , peerPrecommits = Map.empty
                , peerProposals  = Set.empty
                , peerBlocks     = Set.empty
                }
      | otherwise
        -> return peer
    Unknown   -> return Unknown

advancePeer :: (MonadIO m, MonadMask m)
            => PeerStateObj m alg a
            -> FullStep
            -> m ()
advancePeer (PeerStateObj BlockStorage{..} var) (FullStep h r _)
  = modifyMVar_ var modify
  where
    modify peer
      -- Don't go back.
      | Just s <- getPeerStep peer
      , FullStep h r StepNewHeight <= s
      = return peer
      -- If update don't change height only advance step of peer
      | Just h0 <- getPeerHeight peer
      , h0 == h
      = case peer of
          Lagging p -> return $ Lagging p { lagPeerStep = FullStep h r StepNewHeight }
          Current p -> return $ Current p { peerStep    = FullStep h r StepNewHeight }
          Ahead   _ -> return $ Ahead $ FullStep h r StepNewHeight
          Unknown   -> return Unknown
      -- Otherwise we need to set new state of peer
      | otherwise
      = do ourH  <- next <$> blockchainHeight
           case compare h ourH of
             -- FIXME: valid storage MUST return valid answer in that
             --        case but we should handle Nothing case properly
             LT -> do Just vals <- retrieveValidatorSet h
                      Just r    <- if h == Height 0
                                   then return (Just (Round 0))
                                   else retrieveCommitRound h
                      return $ Lagging LaggingPeer
                        { lagPeerStep        = FullStep h r StepNewHeight
                        , lagPeerCommitR     = r
                        , lagPeerValidators  = vals
                        , lagPeerPrecommits  = emptyValidatorISet $ validatorSetSize vals
                        , lagPeerHasProposal = False
                        , lagPeerBlocks      = Set.empty
                        }
             EQ -> do Just vals <- retrieveValidatorSet h
                      return $ Current CurrentPeer
                        { peerStep       = FullStep h r StepNewHeight
                        , peerValidators = vals
                        , peerPrevotes   = Map.empty
                        , peerPrecommits = Map.empty
                        , peerProposals  = Set.empty
                        , peerBlocks     = Set.empty
                        }
             GT -> return $ Ahead $ FullStep h r StepNewHeight

-- | Add proposal to tracked state of peer
addProposal :: (MonadIO m, MonadMask m)
            => PeerStateObj m alg a
            -> Signed ty alg (Proposal alg a)
            -> m ()
addProposal (PeerStateObj _ var) sp@(signedValue -> Proposal{..}) =
  modifyMVar_ var $ \peer -> case peer of
    Lagging p
      | FullStep h _ _ <- lagPeerStep p
      , h == propHeight
      , propRound == lagPeerCommitR p
        -> return $ Lagging p { lagPeerHasProposal = True }
      | otherwise
        -> return peer
    Current p
      | FullStep h _ _ <- peerStep p
      , h == propHeight
        -> return $ Current p { peerProposals = Set.insert propRound (peerProposals p) }
      | otherwise
        -> return peer
    Ahead{}   -> return peer
    Unknown   -> return peer

-- | Add prevote to peer's state given vote itself
addPrevote :: (MonadIO m, MonadMask m)
           => PeerStateObj m alg a
           -> Signed ty alg (Vote 'PreVote alg a)
           -> m ()
addPrevote peer sv@(signedValue -> Vote{..})
  = addPrevoteWrk peer voteHeight voteRound (\vals -> indexByValidator vals (signedAddr sv))

-- | Add prevote to peer's state given peer's vote index.
addPrevoteI :: (MonadIO m, MonadMask m)
            => PeerStateObj m alg a
            -> Height -> Round -> ValidatorIdx alg
            -> m ()
addPrevoteI peer h r i = addPrevoteWrk peer h r (Just . const i)

addPrevoteWrk :: (MonadIO m, MonadMask m)
              => PeerStateObj m alg a
              -> Height -> Round -> (ValidatorSet alg -> Maybe (ValidatorIdx alg))
              -> m ()
addPrevoteWrk (PeerStateObj _ var) h r getI =
  modifyMVar_ var $ \peer -> case peer of
    -- We only send precommits to lagging peers
    Lagging _ -> return peer
    -- Update current peer
    Current p
      | FullStep hPeer _ _ <- peerStep p
      , h == hPeer
      , Just i <- getI (peerValidators p)
      -> return $ Current p { peerPrevotes = Map.alter
                                (\case
                                    Nothing   -> Just
                                               $ insertValidatorIdx i
                                               $ emptyValidatorISet
                                               $ validatorSetSize
                                               $ peerValidators p
                                    Just iset -> Just
                                               $ insertValidatorIdx i iset
                                ) r (peerPrevotes p)
                            }
      | otherwise -> return peer
    -- Nothing to update
    Ahead   _ -> return peer
    Unknown   -> return peer



addPrecommit :: (MonadIO m, MonadMask m)
             => PeerStateObj m alg a
             -> Signed ty alg (Vote 'PreCommit alg a)
             -> m ()
addPrecommit peer sv@(signedValue -> Vote{..})
  = addPrecommitWrk peer voteHeight voteRound (\vals -> indexByValidator vals (signedAddr sv))

addPrecommitI :: (MonadIO m, MonadMask m)
              => PeerStateObj m alg a
              -> Height -> Round -> ValidatorIdx alg
              -> m ()
addPrecommitI peer h r i
  = addPrecommitWrk peer h r (Just . const i)

addPrecommitWrk :: (MonadIO m, MonadMask m)
                => PeerStateObj m alg a
                -> Height -> Round -> (ValidatorSet alg -> Maybe (ValidatorIdx alg))
                -> m ()
addPrecommitWrk (PeerStateObj _ var) h r getI =
  modifyMVar_ var $ \peer -> case peer of
    --
    Lagging p
      | FullStep hPeer rPeer _ <- lagPeerStep p
      , h == hPeer
      , r == rPeer
      , Just i <- getI (lagPeerValidators p)
      -> return $ Lagging p { lagPeerPrecommits = insertValidatorIdx i (lagPeerPrecommits p)
                            }
      | otherwise -> return peer
    --
    Current p
      | FullStep hPeer _ _ <- peerStep p
      , h == hPeer
      , Just i <- getI (peerValidators p)
      -> return $ Current p { peerPrecommits = Map.alter
                                (\case
                                    Nothing   -> Just
                                               $ insertValidatorIdx i
                                               $ emptyValidatorISet
                                               $ validatorSetSize
                                               $ peerValidators p
                                    Just iset -> Just
                                               $ insertValidatorIdx i iset
                                ) r (peerPrecommits p)
                            }
      | otherwise -> return peer
    --
    Ahead _ -> return peer
    Unknown -> return peer

addBlock :: (MonadIO m, MonadMask m, Crypto alg, Serialise a)
         => PeerStateObj m alg a
         -> Block alg a
         -> m ()
addBlock  (PeerStateObj _ var) b =
  modifyMVar_ var $ \peer -> case peer of
    Lagging p -> return $ Lagging p { lagPeerBlocks = Set.insert (blockHash b) (lagPeerBlocks p) }
    Current p -> return $ Current p { peerBlocks    = Set.insert (blockHash b) (peerBlocks p) }
    Ahead _   -> return peer
    Unknown   -> return Unknown

----------------------------------------------------------------

withMVarM :: (MonadMask m, MonadIO m) => MVar a -> (a -> m b) -> m b
withMVarM m action =
  mask $ \restore -> do
    a <- liftIO $ takeMVar m
    b <- restore (action a) `onException` liftIO (putMVar m a)
    liftIO $ putMVar m a
    return b

modifyMVar_ :: (MonadMask m, MonadIO m) => MVar a -> (a -> m a) -> m ()
modifyMVar_ m action =
  mask $ \restore -> do
    a  <- liftIO $ takeMVar m
    a' <- restore (action a) `onException` liftIO (putMVar m a)
    liftIO $ putMVar m a'
