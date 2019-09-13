import Test.Tasty


import qualified TM.Consensus
import qualified TM.Mempool
import qualified TM.Merkle
import qualified TM.MerkleBlock
import qualified TM.Network
import qualified TM.P2P.Gossip
import qualified TM.P2P.PEX
import qualified TM.Serialisation
import qualified TM.Integration
import qualified TM.Time
import qualified TM.Validators

main :: IO ()
main = defaultMain $ testGroup "hschain"
  -- Unit tests
  [ TM.Network.tests
  , TM.Serialisation.tests
  , TM.Time.tests
  , TM.Merkle.tests
  , TM.MerkleBlock.tests
  -- Subsystems tests
  , TM.Mempool.tests
  , TM.P2P.PEX.tests
  , TM.P2P.Gossip.tests
  , TM.Consensus.tests
  -- Integration tests
  , TM.Integration.tests
  -- Unsorted stuff
  , TM.Validators.tests
  ]
