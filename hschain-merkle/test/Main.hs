import Test.Tasty

import qualified TM.Merkle
import qualified TM.MerkleBlock

main :: IO ()
main = defaultMain $ testGroup "hschain-types"
  [ TM.Merkle.tests
  , TM.MerkleBlock.tests
  ]
