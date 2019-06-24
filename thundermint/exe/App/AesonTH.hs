module App.AesonTH(
    A.deriveJSON
  , A.deriveToJSON
  , A.deriveFromJSON
  , dropPrefixOptions
  ) where

import           Data.Aeson.TH (Options(..), SumEncoding(..))
import qualified Data.Aeson.TH as A
import           Data.Char
import           Data.List     (findIndex)
import qualified Data.List     as L





-- | Converts first symbol to lower case
headToLower :: String -> String
headToLower []           = []
headToLower (x:xs@(y:_)) = (if (isLower y) then toLower else id) x : xs
                           -- So that HTTP remains as is
headToLower (x:xs)       = toLower x : xs

-- | Drop prefix of name until first upper letter is occured
stripFieldPrefix :: String -> String
stripFieldPrefix str = case b of
  []   -> dropWhile (not . isUpper) str
  _:xs -> xs
  where
    (_, b) = L.span (/= '\'') str


-- | Drop upper case prefixes from constructor names
--
-- Example:
-- >>> stripConstructorPrefix "ABCombo"
-- "Combo"
--
-- >>> stripConstructorPrefix "Combo"
-- "Combo"
stripConstructorPrefix :: String -> String
stripConstructorPrefix t =
   maybe t (flip drop t . decrementSafe) $ findIndex isLower t
 where
   decrementSafe 0 = 0
   decrementSafe i = i - 1

-- | Options for aeson TH generator, that generates following fields:
--
-- * without punctuation
-- * without lowercase prefix
--
-- And generates constructor tags without uppercase prefixes with
-- 'stripConstructorPrefix'.
--
-- Sums are encoded as one object with only one field corresponding the
-- constructor used.
dropPrefixOptions :: Options
dropPrefixOptions =
   A.defaultOptions
   { fieldLabelModifier = headToLower . stripFieldPrefix
   , constructorTagModifier = stripConstructorPrefix
   , omitNothingFields = True
   , sumEncoding = ObjectWithSingleField
   }
