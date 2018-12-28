{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
-- |
module Thundermint.Blockchain.Internal.Message (
    MessageRx(..)
  , unverifyMessageRx
  , Announcement(..)
  ) where

import Codec.Serialise        (Serialise)
import GHC.Generics           (Generic)

import Thundermint.Crypto
import Thundermint.Blockchain.Types
import Thundermint.Validators


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
  deriving (Show, Generic)
instance (Serialise a) => Serialise (MessageRx 'Unverified alg a)

unverifyMessageRx :: MessageRx 'Verified alg a -> MessageRx 'Unverified alg a
unverifyMessageRx = \case
  RxPreVote   s -> RxPreVote   (unverifySignature s)
  RxPreCommit s -> RxPreCommit (unverifySignature s)
  RxProposal  s -> RxProposal  (unverifySignature s)
  RxTimeout   t -> RxTimeout   t
  RxBlock     b -> RxBlock     b


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
