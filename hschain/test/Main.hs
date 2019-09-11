import Test.Tasty


import qualified TM.Consensus
import qualified TM.Gossip
import qualified TM.Mempool
import qualified TM.Merkle
import qualified TM.MerkleBlock
import qualified TM.Network
import qualified TM.P2P
import qualified TM.Serialisation
import qualified TM.Store
import qualified TM.Time
import qualified TM.Validators

main :: IO ()
main = defaultMain $ testGroup "hschain"
  [ TM.Network.tests
  , TM.P2P.tests
  , TM.Mempool.tests
  , TM.Serialisation.tests
  , TM.Store.tests
  , TM.Time.tests
  , TM.Validators.tests
  , TM.Consensus.tests
  , TM.Gossip.tests
  , TM.Merkle.tests
  , TM.MerkleBlock.tests
  ]
