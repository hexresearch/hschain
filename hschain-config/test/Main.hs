import Test.Tasty
import TM.Parser

main :: IO ()
main = defaultMain $ testGroup "hschain"
  [ TM.Parser.testsMangler
  , TM.Parser.testsParser
  ]


