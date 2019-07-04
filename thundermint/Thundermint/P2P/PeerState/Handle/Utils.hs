
module Thundermint.P2P.PeerState.Handle.Utils where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.Catch      (MonadThrow)
import Control.Monad.RWS.Strict

import Thundermint.Crypto
import Thundermint.Exceptions
import Thundermint.Store
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators

import Thundermint.Control              (throwNothing)
import Thundermint.Store.Internal.Query (MonadReadDB)

import Thundermint.P2P.PeerState.Types

advancePeer :: (CryptoSign alg, CryptoHash alg, Functor m, Monad m, MonadIO m, MonadThrow m, MonadReadDB m alg a)
            => FullStep -> m (SomeState alg a)
advancePeer step@(FullStep h _ _) = do
           ourH <- succ <$> queryRO blockchainHeight
           case compare h ourH of
             -- FIXME: valid storage MUST return valid answer in that
             --        case but we should handle Nothing case properly
             --        (panic)
             LT -> do vals <- throwNothing (DBMissingValSet  h) <=< queryRO
                            $ retrieveValidatorSet h
                      cmtR <- throwNothing (DBMissingRound   h) <=< queryRO
                            $ retrieveCommitRound  h
                      bid  <- throwNothing (DBMissingBlockID h) <=< queryRO
                            $ retrieveBlockID      h
                      return $ wrap $ LaggingState
                        { _lagPeerStep        = step
                        , _lagPeerCommitR     = cmtR
                        , _lagPeerValidators  = vals
                        , _lagPeerPrecommits  = emptyValidatorISet $ validatorSetSize vals
                        , _lagPeerHasProposal = False
                        , _lagPeerHasBlock    = False
                        , _lagPeerBlockID     = bid
                        }
             EQ -> do vals <- throwNothing (DBMissingValSet h) <=< queryRO
                            $ retrieveValidatorSet h
                      return $ wrap $ CurrentState
                        { _peerStep       = step
                        , _peerValidators = vals
                        , _peerPrevotes   = Map.empty
                        , _peerPrecommits = Map.empty
                        , _peerProposals  = Set.empty
                        , _peerBlocks     = Set.empty
                        }
             GT -> return $ wrap $ AheadState step

