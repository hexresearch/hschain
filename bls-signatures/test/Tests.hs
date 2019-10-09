{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- Какие-то непонятные проблемы с Test.Framework
module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Tests.Signature
import {-@ HTF_TESTS @-} Tests.Threshold

main :: IO ()
main = htfMain htf_importedTests

