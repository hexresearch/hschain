import Test.Tasty

import qualified TM.Consensus
import qualified TM.Mempool
import qualified TM.P2P.Gossip
import qualified TM.P2P.PEX
import qualified TM.Serialisation
import qualified TM.Integration
import qualified TM.Validators
import qualified TM.Coin

main :: IO ()
main = defaultMain $ testGroup "hschain"
  -- Unit tests
  [ TM.Serialisation.tests
  -- Subsystems tests
  , TM.Mempool.tests
  , TM.P2P.PEX.tests
  , TM.P2P.Gossip.tests
  , TM.Consensus.tests
  , TM.Coin.tests
  -- Integration tests
  , TM.Integration.tests
  -- Unsorted stuff
  , TM.Validators.tests
  ]
