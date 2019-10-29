import Test.Tasty

import qualified TM.Merkle
import qualified TM.MerkleBlock
import qualified TM.Time

main :: IO ()
main = defaultMain $ testGroup "hschain-types"
  [ TM.Merkle.tests
  , TM.MerkleBlock.tests
  , TM.Time.tests
  ]
