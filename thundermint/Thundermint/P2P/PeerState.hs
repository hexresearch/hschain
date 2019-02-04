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
  , addBlockHR
  ) where

import Codec.Serialise (Serialise)
import Control.Concurrent hiding (modifyMVar_)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Fail

import qualified Data.Map as Map
import           Data.Map   (Map)
import qualified Data.Set as Set
import           Data.Set   (Set)

import Thundermint.Control
import Thundermint.Crypto
import Thundermint.Store
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators


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
  , lagPeerHasBlock    :: !Bool                  -- ^ Whether peer have block
  , lagPeerBlockID     :: BlockID alg a          -- ^ ID of commited block
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

-- | Mutable reference to state of peer. It's safe to mutate
--   concurrently
data PeerStateObj m alg a = PeerStateObj
  !(ProposalStorage 'RO m alg a)
  !(MVar (PeerState alg a))


-- | Create new peer state with default state
newPeerStateObj
  :: MonadReadDB m alg a
  => ProposalStorage 'RO m alg a
  -> m (PeerStateObj m alg a)
newPeerStateObj prop = do
  var <- liftIO $ newMVar Unknown
  return $ PeerStateObj prop var

-- | Obtain current state of peer
getPeerState :: (MonadMask m, MonadIO m)
             => PeerStateObj m alg a -> m (PeerState alg a)
getPeerState (PeerStateObj _ var)
  = withMVarM var return

----------------------------------------------------------------

-- | Local state increased to height H. Update our opinion about
--   peer's state accordingly
advanceOurHeight
  :: (MonadReadDB m alg a, MonadMask m, Crypto alg, Serialise a, MonadFail m)
  => PeerStateObj m alg a
  -> Height
  -> m ()
advanceOurHeight (PeerStateObj _ var) ourH =
  -- Since we (possibly) changed our height following may happen:
  modifyMVarM_ var $ \peer -> case peer of
    -- Lagging peer stays lagging
    Lagging _ -> return peer
    --
    Current p
      -- Current peer may become lagging if we increase our height
      | FullStep h _ _ <- peerStep p
      , h < ourH
        -> do Just vals <- queryRO $ retrieveValidatorSet h
              Just r    <- queryRO $ retrieveCommitRound  h
              Just bid  <- queryRO $ retrieveBlockID      h
              return $ Lagging LaggingPeer
                { lagPeerStep        = peerStep p
                , lagPeerCommitR     = r
                , lagPeerValidators  = pet vals
                , lagPeerPrecommits  = emptyValidatorISet $ validatorSetSize $ pet vals
                , lagPeerHasProposal = r   `Set.member` peerProposals p
                , lagPeerHasBlock    = bid `Set.member` peerBlocks p
                , lagPeerBlockID     = bid
                }
      -- No change otherwise
      | otherwise -> return peer
    -- We may catch up to the peer
    Ahead step@(FullStep h _ _)
      | h == ourH
        -> do Just vals <- queryRO $ retrieveValidatorSet h
              return $ Current CurrentPeer
                { peerStep       = step
                , peerValidators = pet vals
                , peerPrevotes   = Map.empty
                , peerPrecommits = Map.empty
                , peerProposals  = Set.empty
                , peerBlocks     = Set.empty
                }
      | otherwise
        -> return peer
    Unknown   -> return Unknown

advancePeer :: (MonadReadDB m alg a, MonadMask m, Crypto alg, Serialise a, MonadFail m)
            => PeerStateObj m alg a
            -> FullStep
            -> m ()
advancePeer (PeerStateObj _ var) step@(FullStep h _ _)
  = modifyMVarM_ var modify
  where
    modify peer
      -- Don't go back.
      | Just s <- getPeerStep peer
      , step <= s
      = return peer
      -- If update don't change height only advance step of peer
      | Just h0 <- getPeerHeight peer
      , h0 == h
      = case peer of
          Lagging p -> return $ Lagging p { lagPeerStep = step }
          Current p -> return $ Current p { peerStep    = step }
          Ahead   _ -> return $ Ahead step
          Unknown   -> return Unknown
      -- Otherwise we need to set new state of peer
      | otherwise
      = do ourH <- succ <$> queryRO blockchainHeight
           case compare h ourH of
             -- FIXME: valid storage MUST return valid answer in that
             --        case but we should handle Nothing case properly
             --        (panic)
             LT -> do Just vals <- queryRO $ retrieveValidatorSet h
                      Just cmtR <- queryRO $ retrieveCommitRound  h
                      Just bid  <- queryRO $ retrieveBlockID      h
                      return $ Lagging LaggingPeer
                        { lagPeerStep        = step
                        , lagPeerCommitR     = cmtR
                        , lagPeerValidators  = pet vals
                        , lagPeerPrecommits  = emptyValidatorISet $ validatorSetSize $ pet vals
                        , lagPeerHasProposal = False
                        , lagPeerHasBlock    = False
                        , lagPeerBlockID     = bid
                        }
             EQ -> do Just vals <- queryRO $ retrieveValidatorSet h
                      return $ Current CurrentPeer
                        { peerStep       = step
                        , peerValidators = pet vals
                        , peerPrevotes   = Map.empty
                        , peerPrecommits = Map.empty
                        , peerProposals  = Set.empty
                        , peerBlocks     = Set.empty
                        }
             GT -> return $ Ahead step

-- | Add proposal to tracked state of peer
addProposal :: (MonadIO m, MonadMask m)
            => PeerStateObj m alg a
            -> Height
            -> Round
            -> m ()
addProposal (PeerStateObj _ var) hProp rProp =
  modifyMVarM_ var $ \peer -> case peer of
    Lagging p
      | FullStep h _ _ <- lagPeerStep p
      , hProp == h
      , rProp == lagPeerCommitR p
        -> return $ Lagging p { lagPeerHasProposal = True }
      | otherwise
        -> return peer
    Current p
      | FullStep h _ _ <- peerStep p
      , hProp == h
        -> return $ Current p { peerProposals = Set.insert rProp (peerProposals p) }
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
  modifyMVarM_ var $ \peer -> case peer of
    -- We send only precommits to lagging peers
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
  modifyMVarM_ var $ \peer -> case peer of
    --
    Lagging p
      | FullStep hPeer _ _ <- lagPeerStep p
      , h == hPeer
      , r == lagPeerCommitR p
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
         -> Pet (Block alg a)
         -> m ()
addBlock (PeerStateObj _ var) b =
  modifyMVarM_ var $ \peer -> case peer of
    Lagging p
      | bid == lagPeerBlockID p -> return $ Lagging p { lagPeerHasBlock = True }
      | otherwise               -> return peer
    Current p -> return $ Current p { peerBlocks = Set.insert bid (peerBlocks p) }
    Ahead _   -> return peer
    Unknown   -> return Unknown
  where
    bid = blockHash b

addBlockHR :: (MonadIO m, MonadMask m)
           => PeerStateObj m alg a
           -> Height
           -> Round
           -> m ()
addBlockHR (PeerStateObj ProposalStorage{..} var) h r =
  modifyMVarM_ var $ \peer -> case peer of
    Lagging p
      | FullStep hP _ _ <- lagPeerStep p
      , h == hP
      , r == lagPeerCommitR p
        -> return $ Lagging p { lagPeerHasBlock = True }
      | otherwise
        -> return peer
    Current p
      | FullStep hP _ _ <- peerStep p
      , h == hP
        -> retrievePropByR h r >>= \case
             Nothing      -> return peer
             Just (_,bid) -> return $ Current p { peerBlocks = Set.insert bid (peerBlocks p) }
      | otherwise -> return peer
    Ahead _ -> return peer
    Unknown -> return peer
