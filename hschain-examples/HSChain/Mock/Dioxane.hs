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
module HSChain.Mock.Dioxane where

import Codec.Serialise      (Serialise)
import Control.Applicative
import Control.Arrow        ((&&&))
import Control.DeepSeq      (NFData)
import Control.Exception    (Exception)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Except
import Data.Proxy
import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified Data.Vector         as V
import Control.Lens

import GHC.TypeLits
import GHC.Generics (Generic)

import HSChain.Blockchain.Internal.Engine.Types
import HSChain.Control
import HSChain.Crypto
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.Ed25519
import HSChain.Crypto.SHA
import HSChain.Debug.Trace
import HSChain.Logger
import HSChain.Mock.Types
import HSChain.Mock.KeyList
import HSChain.Monitoring
import HSChain.Run
import HSChain.Store
import HSChain.Store.STM
import HSChain.Types
import HSChain.Types.Merkle.Types


----------------------------------------------------------------
-- Basic coin logic
----------------------------------------------------------------

newtype BData tag = BData [Tx]
  deriving stock    (Show,Eq,Generic)
  deriving newtype  (NFData,CryptoHashable,JSON.ToJSON,JSON.FromJSON)
  deriving anyclass (Serialise)

data Tx = Tx
  { txFrom   :: !(PublicKey (Ed25519 :& SHA512))
  , txTo     :: !(PublicKey (Ed25519 :& SHA512))
  , txNonce  :: !Int
  , txAmount :: !Integer
  }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData, JSON.ToJSON, JSON.FromJSON)

data DioError = DioError
  deriving stock    (Show,Generic)
  deriving anyclass (Exception,NFData)

data DioState = DioState
  { _userMap :: Map.Map (PublicKey (Ed25519 :& SHA512)) UserState
  }
  deriving stock    (Show,   Generic)
  deriving anyclass (NFData, Serialise)

data UserState = UserState
  { _userNonce   :: !Int
  , _userBalance :: !Integer
  }
  deriving stock    (Show,   Generic)
  deriving anyclass (NFData, Serialise)

instance CryptoHashable Tx where
  hashStep = genericHashStep "hschain.dioxane"
instance CryptoHashable DioState where
  hashStep = genericHashStep "hschain.dioxane"
instance CryptoHashable UserState where
  hashStep = genericHashStep "hschain.dioxane"


makeLenses ''UserState
makeLenses ''DioState

instance Dio tag => BlockData (BData tag) where
  type TX              (BData tag) = Tx
  type BlockchainState (BData tag) = DioState
  type BChError        (BData tag) = DioError
  type BChMonad        (BData tag) = Maybe
  type Alg             (BData tag) = Ed25519 :& SHA512
  bchLogic                      = dioLogic
  proposerSelection             = ProposerSelection randomProposerSHA512
  blockTransactions (BData txs) = txs
  logBlockData      (BData txs) = HM.singleton "Ntx" $ JSON.toJSON $ length txs


----------------------------------------------------------------
-- Logic
----------------------------------------------------------------

type Tagged a b = Const b a

class Dio a where
  dioUserKeys       :: Tagged a (V.Vector ( PrivKey   (Ed25519 :& SHA512)
                                          , PublicKey (Ed25519 :& SHA512)
                                          ))
  dioInitialBalance :: Tagged a Integer
  dioValidators     :: Tagged a Int

data DioTag (keys :: Nat) (vals :: Nat)

instance (KnownNat keys, KnownNat vals) => Dio (DioTag keys vals) where
  dioUserKeys       = Const
                    $ V.fromList
                    $ take (fromIntegral (natVal (Proxy @keys)))
                    $ map (id &&& publicKey)
                    $ makePrivKeyStream 1337
  dioInitialBalance = Const 1000000
  dioValidators     = Const $ fromIntegral $ natVal (Proxy @vals)

dioGenesis :: forall tag. Dio tag => Genesis (BData tag)
dioGenesis = BChEval
  { bchValue        = makeGenesis (BData []) (hashed state0) valSet valSet
  , validatorSet    = valSet
  , blockchainState = state0
  }
  where
    state0 = DioState
      { _userMap = Map.fromList [ (k, UserState { _userNonce   = 0
                                                , _userBalance = bal
                                                })
                                | (_,k) <- V.toList keys
                                ]
      }
    bal   = getConst (dioInitialBalance @tag)
    nVals = getConst (dioValidators     @tag)
    keys  = getConst (dioUserKeys       @tag)
    Right valSet = makeValidatorSet $  (\(_,k) -> Validator k 1)
                                   <$> V.take nVals keys


dioLogic :: forall tag. Dio tag => BChLogic Maybe (BData tag)
dioLogic = BChLogic
  { processTx     = const empty
  --
  , processBlock  = \BChEval{..} -> do
      st <- foldM (flip process) blockchainState
          $ blockTransactions $ merkleValue $ blockData bchValue
      return BChEval { bchValue        = ()
                     , blockchainState = st
                     , ..
                     }
  -- We generate one transaction for every key. And since we move
  -- money from one account to another it's quite simple to update state
  , generateBlock = \NewBlock{..} _ -> do
      let nonce = let Height h = newBlockHeight in fromIntegral h - 1
          keys  = getConst (dioUserKeys @tag)
      return $ BChEval
        { bchValue = BData [ Tx { txTo     = pk
                                , txFrom   = pk
                                , txNonce  = nonce
                                , txAmount = 1
                                }
                           | (_,pk) <- V.toList keys
                           ]
        , validatorSet    = newBlockValSet
        , blockchainState = newBlockState
                          & userMap . each . userNonce %~ succ
        }
  }



process :: Tx -> DioState -> Maybe DioState
process Tx{..} st = do
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



----------------------------------------------------------------
-- Running it
----------------------------------------------------------------

interpretSpec
  :: forall m x tag.
     ( MonadDB m (BData tag), MonadFork m, MonadMask m, MonadLogger m
     , MonadTrace m, MonadTMMonitoring m, Dio tag
     , Has x BlockchainNet
     , Has x (Configuration Example))
  => x
  -> Int
  -> AppCallbacks m (BData tag)
  -> m (RunningNode m (BData tag), [m ()])
interpretSpec p idx cb = do
  conn    <- askConnectionRO
  store   <- newSTMBchStorage $ blockchainState genesis
  mempool <- makeMempool store run
  acts <- runNode (getT p :: Configuration Example) NodeDescription
    { nodeValidationKey = Just $ PrivValidator $ fst $ getConst (dioUserKeys @tag) V.! idx
    , nodeGenesis       = genesis
    , nodeCallbacks     = cb
    , nodeRunner        = run
    , nodeStore         = AppStore { appBchState = store
                                   , appMempool  = mempool
                                   }
    , nodeNetwork       = getT p
    }
  return
    ( RunningNode { rnodeState   = store
                  , rnodeConn    = conn
                  , rnodeMempool = mempool
                  }
    , acts
    )
  where
    run     = maybe (throwE DioError) return
    genesis = dioGenesis
