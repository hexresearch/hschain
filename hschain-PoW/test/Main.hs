import Test.Tasty

import qualified TM.Consensus
import qualified TM.P2P
import qualified TM.Store

main :: IO ()
main = defaultMain $ testGroup "hschain"
  [ TM.Consensus.tests
  , TM.P2P.tests
  , TM.Store.tests
  ]
