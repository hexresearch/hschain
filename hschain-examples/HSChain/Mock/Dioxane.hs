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
  , inMemoryStateView
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Parallel.Strategies
import qualified Data.Vector         as V

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control.Class
import HSChain.Crypto
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
-- Running it
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


inMemoryStateView
  :: forall tag m. (Monad m, Dio tag)
  => ValidatorSet (Alg (BData tag))
  -> StateView m (BData tag)
inMemoryStateView valSet0 = make Nothing valSet0 (DioState mempty)
  where
    make mh vals st = viewS where
      viewS = StateView
        { stateHeight   = mh
        , newValidators = vals
        , stateMempool  = nullMempool
        , commitState   = return viewS
        -- 
        , validatePropBlock = \b valSet -> return $ maybe (Left DioError) Right $ do
            let sigCheck = guard
                         $ and
                         $ parMap rseq
                           (\(Tx sig tx) -> verifySignatureHashed (txFrom tx) tx sig)
                           (let BData txs = merkleValue $ blockData b
                            in txs
                           )
            let update   = foldM (flip process) st
                         $ (let BData txs = merkleValue $ blockData b
                            in txs
                           )
            st' <- uncurry (>>)
                 $ withStrategy (evalTuple2 rpar rpar)
                 $ (sigCheck, update)
            return $ make (Just $ blockHeight b) valSet st'
        --
        , generateCandidate = \NewBlock{..} -> do
            let nonce = let Height h = newBlockHeight in fromIntegral h - 1
                keys  = dioUserKeys
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
                !st'  = userMap . each . userNonce %~ succ
                      $ st
            return ( txs
                   , make (Just newBlockHeight) newBlockValSet st'
                   )
        }
    DioDict{..} = dioDict @tag


process :: Tx -> DioState -> Maybe DioState
process Tx{txBody=TxBody{..}} st = do
  ufrom  <- st ^. userMap . at txFrom
  _      <- st ^. userMap . at txTo
  -- Nonce is correct & and we have funds
  guard $ txNonce == ufrom^.userNonce
  guard $ ufrom^.userBalance >= txAmount
  return
    $! st
    & userMap . at txFrom . _Just %~ ( (userNonce   %~ succ)
                                     . (userBalance %~ subtract txAmount)
                                     )
    & userMap . at txTo   . _Just . userBalance %~ (+ txAmount)



interpretSpec
  :: forall m tag.
     ( MonadDB (BData tag) m, MonadFork m, MonadMask m, MonadLogger m
     , MonadTMMonitoring m, Dio tag
     )
  => Int
  -> BlockchainNet
  -> Configuration Example
  -> AppCallbacks m (BData tag)
  -> m (StateView m (BData tag), [m ()])
interpretSpec idx bnet cfg cb = do
  acts  <- runNode cfg NodeDescription
    { nodeValidationKey = Just $ PrivValidator $ fst $ dioUserKeys dioD V.! idx
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb
    , nodeStateView     = state
    , nodeNetwork       = bnet
    }
  return
    ( state
    , acts
    )
  where
    state   = inMemoryStateView $ genesisValSet genesis
    genesis = dioGenesis
    dioD    = dioDict @tag
