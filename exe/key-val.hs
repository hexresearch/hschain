{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
import Control.Monad
import Data.Map                 (Map)
import Data.List
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import Text.Groom

import Thundermint.Blockchain.Types
import Thundermint.Consensus.Types
import Thundermint.Crypto
import Thundermint.Crypto.Ed25519   (Ed25519_SHA512)
import Thundermint.P2P.Network
import Thundermint.Store
import Thundermint.Store.STM
import Thundermint.Mock


----------------------------------------------------------------
--
----------------------------------------------------------------

validators :: Map (Address Ed25519_SHA512) (PrivValidator Ed25519_SHA512)
validators = makePrivateValidators
  [ "2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y"
  , "4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL"
  , "3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS"
  , "D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd"
  ]

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
    [ do storage     <- newSTMBlockStorage genesisBlock
         propStorage <- newSTMPropStorage
         let loadAllKeys = Set.fromList . map fst . concatMap blockData <$> loadAllBlocks storage
         return ( createMockNode net "50000" addr
                , map (,"50000") $ connectRing validators addr
                , AppState
                    { appStorage        = storage
                    , appPropStorage    = propStorage
                    --
                    , appValidationFun  = \case
                        [(k,_)] -> do existingKeys <- loadAllKeys
                                      return $ k `Set.notMember` existingKeys
                        _       -> return False
                    , appBlockGenerator =
                        case i of
                          -- Byzantine!
                          0 -> return [("XXX", 0)]
                          _ -> do existingKeys <- loadAllKeys
                                  let Just k = find (`Set.notMember` existingKeys)
                                               ["K_" ++ show (n :: Int) | n <- [1 ..]] 
                                  return [(k,i)]
                    --
                    , appValidator     = val
                    , appValidatorsSet = validatorSet
                    , appMaxHeight     = Just (Height 9)
                    }
                )
    | (i, (addr, val)) <- [0::Int ..] `zip` Map.toList validators
    ]
  st <- runNodeSet nodes
  forM_ st $ \s -> do
    putStrLn "==== BLOCKCHAIN ================================================"
    bs <- loadAllBlocks s
    forM_ bs $ \b -> do
      putStrLn $ groom $ blockHeader b
      print $ blockData b
      putStrLn "----------------"
