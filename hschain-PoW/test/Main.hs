import Test.Tasty

import qualified TM.Consensus

main :: IO ()
main = defaultMain $ testGroup "hschain"
  [ TM.Consensus.tests
  ]
