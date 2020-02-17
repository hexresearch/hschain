import Test.Tasty

import qualified TM.Network

main :: IO ()
main = defaultMain $ testGroup "hschain-net"
  [ TM.Network.tests
  ]
