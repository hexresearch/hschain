{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- |
module TM.Time (tests) where

import Codec.Serialise (Serialise)
import Control.Monad
import Control.Exception
import Data.Int
import Data.List
import Data.Proxy
import qualified Data.ByteString    as BS
import qualified Data.List.NonEmpty as NE
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import GHC.Generics (Generic)

import HSChain.Crypto
import HSChain.Crypto.Ed25519 (Ed25519)
import HSChain.Crypto.SHA     (SHA512)
import HSChain.Crypto.Classes.Hash
import HSChain.Types.Blockchain
import HSChain.Types.Validators
import HSChain.Types.BFTTime


tests :: TestTree
tests = testGroup "time"
  [ testCase "median 2" $ checkMedian [(1,123)] (Just 123)
    -- Even
  , testCase "median even 1" $ checkMedian [(1,10), (1,12)]
                                      (Just 11)
  , testCase "median even 2" $ checkMedian [(2,11), (2,13)]
                                      (Just 12)
  , testCase "median even 3" $ checkMedian [(2,10), (2,12)]
                                      (Just 11)
  , testCase "median even 4" $ checkMedian [(1,10), (1,12), (1,15), (1,20)]
                                      (Just 13)
  , testCase "No overflow odd"  $ checkMedian [ (1, maxBound - 3)
                                              , (1, maxBound - 5)
                                              ] (Just $ maxBound - 4)
  , testCase "No overflow even" $ checkMedian [ (1, maxBound - 2)
                                              , (1, maxBound - 4)
                                              ] (Just $ maxBound - 3)
    -- Odd
  , testCase "median odd 1" $ checkMedian [(1,10), (1,12), (1, 13)]
                                (Just 12)
  , testCase "median odd 2" $ checkMedian [(2,10), (1,12)]
                                (Just 10)
  , testCase "median odd 3" $ checkMedian [(1,10), (2,12)]
                                (Just 12)
  ]

checkMedian :: [(Integer,Int64)] -> Maybe Int64 -> IO ()
checkMedian wtimes expectedT = forM_ (permuteCommit commit) $ \cmt ->
  assertEqual
    (show $ voteTime . signedValue <$> commitPrecommits cmt)
    (fmap Time expectedT)
    (commitTime valSet (Time 0) cmt)
  where
    Right valSet = makeValidatorSet
      [ Validator (publicKey pk) w
      | ((w,_),pk) <- wtimes `zip` privateKeyList
      ]
    commit = Commit
      { commitBlockID    = bid
      , commitPrecommits = NE.fromList
          [ signValue idx pk Vote
              { voteHeight  = Height 1
              , voteRound   = Round 0
              , voteBlockID = Just bid
              , voteTime    = Time t
              }
          | ((_,t), pk) <- wtimes `zip` privateKeyList
          , let Just idx = indexByValidator valSet (publicKey pk)
          ]
      }

permuteCommit :: Commit a -> [Commit a]
permuteCommit Commit{..} =
  [ Commit { commitPrecommits = pc
           , ..
           }
  | pc <- NE.fromList <$> permutations (NE.toList commitPrecommits)
  ]


data BData = BData
  deriving stock    (Show,Eq,Generic)
  deriving anyclass (Serialise)

instance CryptoHashable BData where
  hashStep = genericHashStep "hschain-tests"

data Err = Err
  deriving stock (Show)
  deriving anyclass (Exception)

instance BlockData BData where
  type TX              BData = ()
  type BlockchainState BData = ()
  type BChError        BData = Err
  type Alg             BData = Ed25519 :& SHA512
  type BChMonad        BData = Maybe
  proposerSelection   = error "proposerSelection is unused"
  bchLogic            = error "bchLogic is unused"


bid :: BlockID BData
bid = BlockID (Hashed (hash BData))

privateKeyList :: [PrivKey (Alg BData)]
privateKeyList = makePrivKeyStream 13337

-- Sadly (code duplication. Same function is defined in hschain-examples)
makePrivKeyStream :: forall alg. CryptoSign alg => Int -> [PrivKey alg]
makePrivKeyStream seed
  = unfoldr step
  $ randoms (mkStdGen seed)
  where
    keySize     = privKeySize (Proxy @alg)
    step stream = Just (k, stream')
      where
        Just k        = decodeFromBS $ BS.pack bs
        (bs, stream') = splitAt keySize stream
