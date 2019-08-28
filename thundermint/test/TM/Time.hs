{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
-- |
module TM.Time (tests) where

import Control.Monad
import Data.Int
import Data.List
import qualified Data.List.NonEmpty as NE
import Test.Tasty
import Test.Tasty.HUnit

import Thundermint.Crypto
import Thundermint.Crypto.Ed25519 (Ed25519)
import Thundermint.Crypto.SHA     (SHA512)
import Thundermint.Mock.KeyList (privateKeyList)
import Thundermint.Types.Blockchain
import Thundermint.Types.Validators
import Thundermint.Types.BFTTime


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

permuteCommit :: Commit alg a -> [Commit alg a]
permuteCommit Commit{..} =
  [ Commit { commitPrecommits = pc
           , ..
           }
  | pc <- NE.fromList <$> permutations (NE.toList commitPrecommits)
  ]

bid :: BlockID (Ed25519 :& SHA512) ()
bid = BlockID (Hashed (hash ()))
