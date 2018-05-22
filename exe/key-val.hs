{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
import Control.Monad
import Data.Map                 (Map)
import Data.List
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import Text.Groom

import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.Crypto.Ed25519   (Ed25519_SHA512, privateKey)
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.STM
import Thundermint.Mock


----------------------------------------------------------------
--
----------------------------------------------------------------

{-
-}
-- FIXME: replace base16 with base58

fromBase16 :: BS.ByteString -> BS.ByteString
fromBase16 = fst . Base16.decode

-- FIXME: keys show be stored somewhere

validators :: Map BS.ByteString (PrivValidator Ed25519_SHA512)
validators = Map.fromList
  [ n .= PrivValidator { validatorPrivKey  = privateKey $ fromBase16 n
                       }
  | n <- [ "137f97f2a73e576b8b8d52b3728088ac6c25383065853b5d049da74100f6a2db"
         , "32111ba438148948ab3119f4f2132530ec844de1bf2521fa840555a9afcf15dd"
         , "217d248f6623d335692d6198a6121ae24e6dac97c1e70ea60e0ce4ee2099d7b5"
         , "b2b9b660e40438ed7a4e3d05b108eec78c4915f91982bb480d6a06109a01f7de"
         ]
  ]
  where (.=) = (,)

genesisBlock :: Block Ed25519_SHA512 [(String,Int)]
genesisBlock = Block
  { blockHeader = Header
      { headerChainID     = "KV"
      , headerHeight      = Height 0
      , headerTime        = Time 0
      , headerLastBlockID = Nothing
      }
  , blockData       = []
  , blockLastCommit = Nothing
  }


loadAllBlocks :: Monad m => BlockStorage ro m alg a -> m [Block alg a]
loadAllBlocks storage = go (Height 0)
  where
    go h = retrieveBlock storage h >>= \case
      Nothing -> return []
      Just b  -> (b :) <$> go (next h)


-- Key-value demo blockchain.
--
-- Blockchain is append only key-value map. Each block contains
-- several key-value pairs. Following constraints apply:
--
--  * Block must contain only one pair
--  * Reuse of key is not possible.
main :: IO ()
main = do
  let validatorSet = makeValidatorSet validators
  net   <- newMockNet
  nodes <- sequence
    [ do storage <- newSTMBlockStorage genesisBlock
         let loadAllKeys = Set.fromList . map fst . concatMap blockData <$> loadAllBlocks storage
         return ( createMockNode net addr
                , map (,"50000") $ connectRing validators addr
                , AppState
                    { appStorage        = storage
                    --
                    , appValidationFun  = \case
                        [(k,_)] -> do existingKeys <- loadAllKeys
                                      return $ k `Set.notMember` existingKeys
                        _       -> return False
                    , appBlockGenerator = do
                        existingKeys <- loadAllKeys
                        let keys   = ["K_" ++ show (n :: Int) | n <- [1 ..]]
                            Just k = find (`Set.notMember` existingKeys) keys
                        return [(k,1)]
                    --
                    , appValidator     = val
                    , appValidatorsSet = validatorSet
                    , appMaxHeight     = Just (Height 3)
                    }
                )
         | (addr, val) <- Map.toList validators
         ]
  st <- runNodeSet nodes
  forM_ st $ \s -> do
    putStrLn "==== BLOCKCHAIN ================================================"
    bs <- loadAllBlocks s
    forM_ bs $ \b -> do
      putStrLn $ groom $ blockHeader b
      print $ blockData b
      putStrLn "----------------"
