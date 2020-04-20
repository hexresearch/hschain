import Test.Tasty

import qualified TM.Consensus
import qualified TM.P2P

main :: IO ()
main = defaultMain $ testGroup "hschain"
  [ TM.Consensus.tests
  , TM.P2P.tests
  ]
