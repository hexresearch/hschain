{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-- |
-- Simple account based "cryptocurrency". It was created mostly for
-- benchmarking so one of concerns is easy of generation of block.
module HSChain.Mock.Dioxane (
    module HSChain.Mock.Dioxane.Types
  , dioGenesis
  , interpretSpec
  , DioState(..)
  , UserState(..)
    -- * Lens
  , userNonce, userBalance, userMap, dioHeight, dioValSet
  ) where

import Codec.Serialise (Serialise)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Parallel.Strategies
import Data.Int
import qualified Data.Vector         as V
import qualified Data.Map.Strict     as Map
import GHC.Generics (Generic)

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control.Class
import HSChain.Crypto
import HSChain.Crypto.Classes.Hash (genericHashStep)
import HSChain.Logger
import HSChain.Mempool
import HSChain.Internal.Types.Consensus
import HSChain.Mock.Types
import HSChain.Mock.Dioxane.Types
import HSChain.Monitoring
import HSChain.Run
import HSChain.Store
import HSChain.Types
import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- State & genesis
----------------------------------------------------------------

dioGenesis :: forall tag. Dio tag => Genesis (BData tag)
dioGenesis = Genesis
  { genesisBlock  = makeGenesis (BData []) valSet valSet
  , genesisValSet = valSet
  }
  where
    DioDict{..}  = dioDict @tag
    nVals        = dioValidators
    keys         = dioUserKeys
    Right valSet = makeValidatorSet $  (\(_,k) -> Validator k 1)
                                   <$> V.take (fromIntegral nVals) keys

data DioState tag = DioState
  { _dioHeight :: Maybe Height
  , _dioValSet :: ValidatorSet DioAlg
  , _userMap   :: Map.Map (PublicKey DioAlg) UserState
  }
  deriving stock    (Show,   Generic)
  deriving anyclass (NFData, Serialise)

data UserState = UserState
  { _userNonce   :: !Int64
  , _userBalance :: !Int64
  }
  deriving stock    (Show,   Generic)
  deriving anyclass (NFData, Serialise)

instance CryptoHashable (DioState tag) where
  hashStep = genericHashStep "hschain.dioxane"
instance CryptoHashable UserState where
  hashStep = genericHashStep "hschain.dioxane"

makeLenses ''UserState
makeLenses ''DioState


----------------------------------------------------------------
-- State implementation
----------------------------------------------------------------

instance Dio tag => StateView (DioState tag) where
  type BlockType       (DioState tag) = BData tag
  type ViewConstraints (DioState tag) = Applicative
  stateHeight       = _dioHeight
  newValidators     = _dioValSet
  commitState       = pure
  validatePropBlock st b valSet = pure $ maybe (Left DioError) Right $ do
    let sigCheck = guard
                 $ and
                 $ parMap rseq
                   (\(Tx sig tx) -> verifySignatureHashed (txFrom tx) tx sig)
                   (let BData txs = merkleValue $ blockData b
                    in txs
                   )
    let update   = foldM (flip process) (st^.userMap)
                 $ (let BData txs = merkleValue $ blockData b
                    in txs
                   )
    st' <- uncurry (>>)
         $ withStrategy (evalTuple2 rpar rpar)
         $ (sigCheck, update)
    return $ DioState { _dioHeight = Just $ blockHeight b
                      , _dioValSet = valSet
                      , _userMap   = st'
                      }
  generateCandidate st NewBlock{..} = do
    let nonce = let Height h = newBlockHeight in fromIntegral h - 1
        keys  = dioUserKeys (dioDict @tag)
    let !txs  = BData
              $ parMap rseq
                   (\(sk,pk) -> let body = TxBody
                                      { txTo     = pk
                                      , txFrom   = pk
                                      , txNonce  = nonce
                                      , txAmount = 1
                                      }
                                in Tx { txSig  = signHashed sk body
                                      , txBody = body
                                      }
                   )
                   (V.toList keys)
        !st'  = dioHeight .~ Just newBlockHeight
              $ dioValSet .~ newBlockValSet
              $ userMap . each . userNonce %~ succ
              $ st
    pure ( txs, st')

process :: Tx -> Map.Map (PublicKey DioAlg) UserState -> Maybe (Map.Map (PublicKey DioAlg) UserState)
process Tx{txBody=TxBody{..}} st = do
  ufrom  <- st ^. at txFrom
  _      <- st ^. at txTo
  -- Nonce is correct & and we have funds
  guard $ txNonce == ufrom^.userNonce
  guard $ ufrom^.userBalance >= txAmount
  return
    $! st
    & at txFrom . _Just %~ ( (userNonce   %~ succ)
                           . (userBalance %~ subtract txAmount)
                           )
    & at txTo   . _Just . userBalance %~ (+ txAmount)

interpretSpec
  :: forall m tag.
     ( MonadDB m, MonadCached (BData tag) m, MonadFork m, MonadMask m, MonadLogger m
     , MonadTMMonitoring m, Dio tag
     )
  => Int
  -> BlockchainNet
  -> Configuration Example
  -> AppCallbacks m (BData tag)
  -> m (DioState tag, [m ()])
interpretSpec idx bnet cfg cb = do
  acts  <- runNode cfg NodeDescription
    { nodeValidationKey = Just $ PrivValidator $ fst $ dioUserKeys dioD V.! idx
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb
    , nodeStateView     = state
    , nodeNetwork       = bnet
    }
    (nullMempool hashed)
  return
    ( state 
    , acts
    )
  where
    state   = DioState { _dioHeight = Nothing
                       , _dioValSet = genesisValSet genesis
                       , _userMap   = mempty
                       }
    genesis = dioGenesis
    dioD    = dioDict @tag

