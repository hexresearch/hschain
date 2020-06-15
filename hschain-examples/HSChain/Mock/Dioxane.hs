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
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Except
import qualified Data.Map.Strict     as Map
import qualified Data.Vector         as V

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control.Class
import HSChain.Crypto
import HSChain.Logger
import HSChain.Mempool
import HSChain.Mock.Types
import HSChain.Mock.Dioxane.Types
import HSChain.Monitoring
import HSChain.Run
import HSChain.Store
import HSChain.Store.STM
import HSChain.Types
import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- Running it
----------------------------------------------------------------

dioGenesis :: forall tag. Dio tag => Genesis (BData tag)
dioGenesis = BChEval
  { bchValue        = makeGenesis (BData []) (hashed state0) valSet valSet
  , validatorSet    = merkled valSet
  , blockchainState = merkled state0
  }
  where
    state0  = DioState
      { _userMap = Map.fromList [ (k, UserState { _userNonce   = 0
                                                , _userBalance = bal
                                                })
                                | (_,k) <- V.toList keys
                                ]
      }
    DioDict{..} = dioDict @tag
    bal         = dioInitialBalance
    nVals       = dioValidators
    keys        = dioUserKeys
    Right valSet = makeValidatorSet $  (\(_,k) -> Validator k 1)
                                   <$> V.take (fromIntegral nVals) keys

interpretSpec
  :: forall m tag.
     ( MonadDB (BData tag) m, MonadFork m, MonadMask m, MonadLogger m
     , MonadTMMonitoring m, Dio tag
     )
  => Int
  -> BlockchainNet
  -> Configuration Example
  -> AppCallbacks m (BData tag)
  -> m (RunningNode m (BData tag), [m ()])
interpretSpec idx bnet cfg cb = do
  conn    <- askConnectionRO
  store   <- newSTMBchStorage (blockchainState genesis)
  acts <- runNode cfg NodeDescription
    { nodeValidationKey = Just $ PrivValidator $ fst $ dioUserKeys dioD V.! idx
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb
    , nodeRunner        = run
    , nodeStore         = AppStore { appBchState = store
                                   , appMempool  = nullMempool
                                   }
    , nodeNetwork       = bnet
    }
  return
    ( RunningNode { rnodeState   = store
                  , rnodeConn    = conn
                  , rnodeMempool = nullMempool
                  }
    , acts
    )
  where
    run     = maybe (throwE DioError) return
    genesis = dioGenesis
    dioD    = dioDict @tag
