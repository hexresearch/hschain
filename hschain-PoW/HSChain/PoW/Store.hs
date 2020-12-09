-- |
-- API and default implementation for storage of blocks. Normally
-- blocks are meant to be stored in the database but for testing and
-- debugging it could be useful to have in-memory storage as well.
module HSChain.PoW.Store
  ( -- * API and generic functions
    BlockDB(..)
  , buildBlockIndex
    -- * Implementations
  , inMemoryDB
  , blockDatabase
  ) where

import Codec.Serialise (Serialise)
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Foldable
import Data.Functor.Identity
import Data.IORef
import Data.List (sortOn)
import Data.Proxy
import qualified Data.Map.Strict as Map

import HSChain.Types.Merkle.Types
import HSChain.Store.Query
import HSChain.PoW.Types
import HSChain.PoW.BlockIndex


----------------------------------------------------------------
-- Block database
----------------------------------------------------------------

-- | API for append only block storage. It should always contain
--   genesis block.
data BlockDB m b = BlockDB
  { storeHeader :: Header b -> m ()
    -- ^ Pure header in storage. Operation should be idempotent.
  , storeBlock :: Block b -> m ()
    -- ^ Put block into storage. It should be idempotent.
  , retrieveBlock :: BlockID b -> m (Maybe (Block  b))
    -- ^ Retrive complete block by its identifier
  , retrieveHeader :: BlockID b -> m (Maybe (Header b))
    -- ^ Retrieve header by its identifier
  , retrieveAllHeaders :: m [Header b]
    -- ^ Retrieve all headers from storage in topologically sorted
    --   order. (Ordering block by height would achieve that for
    --   example).
  }

-- | Build block index from blocks
buildBlockIndex :: (Monad m, BlockData b) => BlockDB m b -> m (BlockIndex b)
buildBlockIndex BlockDB{..} = do
  -- FIXME: decide what to do with orphan blocks
  (genesis,headers) <- retrieveAllHeaders >>= \case
    genesis:headers -> return (genesis,headers)
    []              -> error "buildBlockIndex: no blocks in storage"
  --
  let idx0      = blockIndexFromGenesis genesis
      add idx b@Block{..} = case (`lookupIdx` idx) =<< prevBlock of
        Nothing     -> error "blockIndexFromGenesis: orphan block"
        Just parent -> insertIdx BH
          { bhHeight   = blockHeight
          , bhTime     = blockTime
          , bhBID      = blockID b
          , bhWork     = bhWork parent <> blockWork b
          , bhPrevious = Just parent
          , bhData     = blockData
          } idx
  return $! foldl' add idx0 headers


----------------------------------------------------------------
-- Implementations
----------------------------------------------------------------

-- | Very simple block storage which holds all blocks in memory. Only
--   useful for testing, debugging, and prototyping.
inMemoryDB
  :: (MonadIO m, MonadIO n, BlockData b)
  => Block b
  -> m (BlockDB n b)
inMemoryDB genesis = do
  var <- liftIO $ newIORef $ Map.singleton (blockID genesis) (Right genesis)
  return BlockDB
    { storeHeader    = \h   -> liftIO $ modifyIORef' var $
        Map.alter (\mb -> mb <|> Just (Left h)) (blockID h)
    , storeBlock     = \b   -> liftIO $ modifyIORef' var $
        Map.insert (blockID b) (Right b)
    , retrieveBlock  = \bid -> liftIO $ do m <- readIORef var
                                           pure $ do Right b <- Map.lookup bid m
                                                     Just  b
    , retrieveHeader = \bid -> liftIO $ do m <- readIORef var
                                           pure $ case Map.lookup bid m of
                                                    Just (Left h)  -> Just h
                                                    Just (Right b) -> Just (toHeader b)
                                                    Nothing        -> Nothing
    , retrieveAllHeaders = liftIO $ sortOn blockHeight
                                  . map (either id toHeader)
                                  . toList <$> readIORef var
    }



-- | Block storage backed by SQLite database. It's most generic
--   storage which just stores
blockDatabase
  :: ( MonadThrow m, MonadIO m, MonadDB m
     , BlockData b, Serialise (b Proxy), Serialise (b Identity))
  => Block b
  -> m (BlockDB m b)
blockDatabase genesis = do
  -- First we initialize database schema
  mustQueryRW $ basicExecute_
    "CREATE TABLE IF NOT EXISTS pow_blocks \
    \  ( id         INTEGER PRIMARY KEY AUTOINCREMENT \
    \  , bid        BLOB NOT NULL UNIQUE \
    \  , height     INTEGER NOT NULL \
    \  , time       INTEGER NUT NULL \
    \  , prev       BLOB NULL \
    \  , headerData BLOB NOT NULL \
    \  , blockData  BLOB NULL )"
  storeB genesis
  return BlockDB
    { storeHeader = storeH
    , storeBlock  = storeB
      --
    , retrieveBlock  = \bid -> queryRO $ basicQueryWith1
        decoderBlock
        "SELECT height, time, prev, blockData FROM pow_blocks \
        \ WHERE bid = ? AND blockData IS NOT NULL"
        (Only (CBORed bid))
      --
    , retrieveHeader = \bid -> queryRO $ basicQueryWith1
        decoderHeader
        "SELECT height, time, prev, headerData FROM pow_blocks WHERE bid = ?"
        (Only (CBORed bid))
      --
    , retrieveAllHeaders = queryRO $ basicQueryWith_
        decoderHeader
        "SELECT height, time, prev, headerData FROM pow_blocks ORDER BY height"
    }
    where
      decoderBlock  = do blockHeight <- field
                         blockTime   <- field
                         prevBlock   <- nullableFieldCBOR
                         blockData   <- fieldCBOR
                         return Block{..}
      decoderHeader = do blockHeight <- field
                         blockTime   <- field
                         prevBlock   <- nullableFieldCBOR
                         blockData   <- fieldCBOR
                         return Block{..}
      --
      storeH b@Block{..} = mustQueryRW $ basicExecute
        "INSERT OR IGNORE INTO pow_blocks VALUES (NULL, ?, ?, ?, ?, ?, NULL)"
        ( CBORed (blockID b)
        , blockHeight
        , blockTime
        , CBORed <$> prevBlock
        , CBORed (merkleMap (const Proxy) blockData)
        )
      storeB b@Block{..} = mustQueryRW $ do
        exists <- basicQuery
          "SELECT blockData IS NULL FROM pow_blocks WHERE bid = ?"
          (Only (CBORed bid))
        case exists of
          [] -> basicExecute
                "INSERT OR IGNORE INTO pow_blocks VALUES (NULL, ?, ?, ?, ?, ?, ?)"
                ( CBORed bid
                , blockHeight
                , blockTime
                , CBORed <$> prevBlock
                , CBORed (merkleMap (const Proxy) blockData)
                , CBORed blockData
                )
          [Only True] -> basicExecute
               "UPDATE pow_blocks SET blockData = ? WHERE bid = ?"
               (CBORed blockData, CBORed bid)
          _ -> return ()
        where
          bid = blockID b
