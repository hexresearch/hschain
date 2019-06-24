module App.Yaml(
    loadYaml
  , readYaml
  , readYaml'
) where

import qualified Control.Exception as Exception
import           Data.Aeson
import           Data.ByteString   (ByteString)
import           Data.ByteString   as BS (readFile)
import           Data.Yaml         (decodeEither')
import           Data.Yaml.Config  (loadYamlSettings, useEnv)

loadYaml ::  FromJSON settings => ByteString -> IO settings
loadYaml bs = loadYamlSettings [] [value] useEnv
  where
    value = either Exception.throw id $ decodeEither' bs

readYaml' :: FromJSON settings => FilePath -> IO settings
readYaml' fp = loadYaml =<< BS.readFile fp

readYaml :: FromJSON settings => FilePath -> IO settings
readYaml fp = loadYamlSettings [fp] [] useEnv
