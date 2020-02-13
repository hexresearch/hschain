-- | Stub for some 'realNetwork' implementations
module HSChain.P2P.Network.RealNetworkStub
    ( -- * Real network stub
      realNetworkStub
    ) where

import Data.Word
import HSChain.P2P.Types

realNetworkStub :: Word16 -> NetworkAPI
realNetworkStub port = NetworkAPI
  { listenOn    = undefined
  , connect     = undefined
  , listenPort  = fromIntegral port
  }
