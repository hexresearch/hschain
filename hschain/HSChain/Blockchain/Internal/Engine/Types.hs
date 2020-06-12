{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Data types for storage of blockchain
module HSChain.Blockchain.Internal.Engine.Types (
    -- * Application state
    AppLogic
  , AppStore(..)
  , AppCallbacks(..)
  , Validator(..)
  , PrivValidator(..)
    -- * Messages and channels
  , MessageRx(..)
  , unverifyMessageRx
  , Announcement(..)
  , AppChans(..)
    -- * Proposers
  , randomProposerSHA512
    -- * Logging
  , LogBlockInfo(..)
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Coerce
import Data.Bits              (shiftL)
import Data.Monoid            (Any(..))
import Data.Maybe             (fromMaybe)
import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HM
import qualified Katip

import HSChain.Blockchain.Internal.Types
import HSChain.Crypto
import HSChain.Crypto.SHA (SHA512)
import HSChain.Mempool
import HSChain.Store
import HSChain.Types.Blockchain
import HSChain.Types.Validators
import HSChain.Internal.Types.Messages


----------------------------------------------------------------
--
----------------------------------------------------------------

type AppLogic m a = BChLogic (ExceptT (BChError a) m) a

data AppStore m a = AppStore
  { appMempool          :: Mempool m (Alg a) (TX a)
    -- ^ Application mempool
  , appBchState         :: BChStore m a
    -- ^ Store for the blockchain state
  }

-- | User callbacks which have monoidal strcture
data AppCallbacks m a = AppCallbacks
  { appCommitCallback   :: Block a -> m ()
    -- ^ Function which is called after each commit.
  , appCanCreateBlock   :: Height -> m (Maybe Bool)
    -- ^ Callback which is called to decide whether we ready to create
    --   new block or whether we should wait
  }

instance Monad m => Semigroup (AppCallbacks m a) where
  AppCallbacks f1 g1 <> AppCallbacks f2 g2 = AppCallbacks
    { appCommitCallback = liftA2 (*>) f1 f2
    , appCanCreateBlock = (liftA2 . liftA2) (coerce ((<>) @(Maybe Any))) g1 g2
    }
instance Monad m => Monoid (AppCallbacks m a) where
  mempty  = AppCallbacks (\_ -> pure ()) (\_ -> pure Nothing)

instance HoistDict AppCallbacks where
  hoistDict fun AppCallbacks{..} = AppCallbacks
    { appCommitCallback = fun . appCommitCallback
    , appCanCreateBlock = fun . appCanCreateBlock
    }

-- | Application connection to outer world
data AppChans a = AppChans
  { appChanRx  :: TBQueue (MessageRx 'Unverified a)
    -- ^ Queue for receiving messages related to consensus protocol
    --   from peers.
  , appChanTx  :: TChan (MessageTx a)
    -- ^ TChan for broadcasting messages to the peers
  , appTMState :: TVar  (Maybe (Height, TMState a))
    -- ^ Current state of consensus. It includes current height, state
    --   machine status and known blocks which should be exposed in
    --   read-only manner for gossip with peers.
  }


-- | Select proposers using PRNG based on SHA512.
randomProposerSHA512 :: Crypto alg => ValidatorSet alg -> Height -> Round -> ValidatorIdx alg
randomProposerSHA512 valSet h r
  = fromMaybe (error "randomProposerSHA512: invalid index")
  $ indexByIntervalPoint valSet
  $ fromInteger
  -- NOTE: We just compute modulo total voting power. This gives
  --       _biased_ results. But since range of SHA512 is enormous:
  --       2^512 even for voting power on order 2^64 bias will be on
  --       order 10^{-134} that is negligible
  $ (`mod` fromIntegral (totalVotingPower valSet))
  -- Convert hash to integer. We interpret hash as LE integer
  $ BS.foldr' (\w i -> (i `shiftL` 8) + fromIntegral  w) 0 bs
  where
    Hash bs = hash (valSet, h, r) :: Hash SHA512


-----------------------------------------------------------------
--- LogBlock
-----------------------------------------------------------------

-- | Wrapper for log data for logging purposes
data LogBlockInfo a = LogBlockInfo !Height !a !Int

instance BlockData a => Katip.ToObject (LogBlockInfo a) where
  toObject (LogBlockInfo (Height h) a ns)
    = HM.insert "H"     (toJSON h)
    $ HM.insert "nsign" (toJSON ns)
    $ logBlockData a

instance BlockData a => Katip.LogItem (LogBlockInfo a) where
  payloadKeys Katip.V0 _ = Katip.SomeKeys ["H"]
  payloadKeys _        _ = Katip.AllKeys
