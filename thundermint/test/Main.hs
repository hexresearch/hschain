import Test.Tasty


import qualified TM.Network
import qualified TM.NetworkTls
import qualified TM.P2P
import qualified TM.Mempool
import qualified TM.Serialisation
import qualified TM.Store
import qualified TM.Persistence
import qualified TM.Time

main :: IO ()
main = defaultMain $ testGroup "test suite"
  [ TM.Network.tests
  {-, TM.P2P.tests
  , TM.NetworkTls.tests
  , TM.Mempool.tests
  , TM.Persistence.tests
  , TM.Serialisation.tests
  , TM.Store.tests
  , TM.Time.tests-}
  ]
