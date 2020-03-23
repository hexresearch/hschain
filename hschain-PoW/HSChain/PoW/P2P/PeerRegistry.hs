-- |
module HSChain.PoW.P2P.PeerRegistry
  (
  )where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Set        (Set)
import Data.Time

import HSChain.Network.Types


-- | Concurrent data structure that tracks state of peers
data PeerRegistry = PeerRegistry
  { prConnected     :: !(TVar (Set NetAddr))
    -- ^ Set of peers to which we're connected
  , prKnownAddreses :: !(TVar (Set NetAddr))
    -- ^ Set of known good addresses
  , prBanned        :: !(TVar (Map NetAddr UTCTime))
    -- ^ 
  , prSelfAddresses :: !(TVar (Set NetAddr))
    -- ^ Set of our own addresses.
  }
