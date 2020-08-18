import Test.Tasty

import qualified TM.Consensus
import qualified TM.Coin
import qualified TM.Mempool
import qualified TM.P2P
import qualified TM.Store

main :: IO ()
main = defaultMain $ testGroup "hschain"
  [ TM.Consensus.tests
  , TM.P2P.tests
  , TM.Store.tests
  , TM.Mempool.tests
  , TM.Coin.tests
  ]
