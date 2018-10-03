{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
-- |
module Thundermint.Blockchain.Message (
    MessageRx(..)
  , Announcement(..)
  ) where

import Codec.Serialise        (Serialise)
import Control.Concurrent.STM
import qualified Data.Aeson as JSON
import GHC.Generics           (Generic)

import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Consensus.Types


----------------------------------------------------------------
-- Messages
----------------------------------------------------------------

-- | Message received by main application
data MessageRx ty alg a
  = RxPreVote   !(Signed ty alg (Vote 'PreVote   alg a))
  | RxPreCommit !(Signed ty alg (Vote 'PreCommit alg a))
  | RxProposal  !(Signed ty alg (Proposal alg a))
  | RxTimeout   !Timeout
  | RxBlock     !(Block alg a)
  deriving (Show)

-- | Messages which should be delivered to peers immediately. Those
--   are control messages in gossip protocol. Actual proposals, votes
--   and blocks are delivered pure via gossip.
data Announcement alg
  = AnnStep         !FullStep
  | AnnHasProposal  !Height !Round
  | AnnHasBlock     !Height !Round
  | AnnHasPreVote   !Height !Round !(ValidatorIdx alg)
  | AnnHasPreCommit !Height !Round !(ValidatorIdx alg)
  deriving (Show,Generic)
instance Serialise (Announcement alg)
