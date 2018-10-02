{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
module Thundermint.Mock.Store (
   newBlockStorage
 ) where

import Codec.Serialise  (Serialise)
import System.Directory (createDirectoryIfMissing)
import System.FilePath  (splitFileName, (</>))

import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Crypto.Containers
import Thundermint.Store
import Thundermint.Store.SQLite
import Thundermint.Store.STM

-- | Create block storage. It will use SQLite if path is specified or
--   STM otherwise.
newBlockStorage
  :: (Crypto alg, Serialise a)
  => FilePath                   -- ^ Prefix
  -> Maybe FilePath             -- ^ Path to database
  -> Block alg a
  -> ValidatorSet alg
  -> IO (BlockStorage 'RW IO alg a)
newBlockStorage prefix mpath genesis validatorSet = do
  let makedir path = let (dir,_) = splitFileName path
                     in createDirectoryIfMissing True dir
  case mpath of
      Nothing -> newSQLiteBlockStorage ":memory:" genesis validatorSet
      Just nm -> do
        let dbName = prefix </> nm
        makedir dbName
        newSQLiteBlockStorage dbName genesis validatorSet
