import Test.Tasty

import qualified TM.Time

main :: IO ()
main = defaultMain $ testGroup "hschain-types"
  [ TM.Time.tests
  ]
