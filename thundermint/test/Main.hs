import Test.Tasty


import qualified TM.Network
import qualified TM.NetworkTls
import qualified TM.P2P
import qualified TM.Mempool
import qualified TM.Serialisation
import qualified TM.Store
import qualified TM.Time
import qualified TM.Validators

main :: IO ()
main = defaultMain $ testGroup "test suite"
  [ TM.Network.tests
  , TM.P2P.tests
  , TM.NetworkTls.tests
  , TM.Mempool.tests
  , TM.Serialisation.tests
  , TM.Store.tests
  , TM.Time.tests
  , TM.Validators.tests
  ]
