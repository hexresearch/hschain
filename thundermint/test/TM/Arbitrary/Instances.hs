{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Arbitrary instances for QuickTest
--
module TM.Arbitrary.Instances where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Thundermint.P2P.Types (NetAddr(..))

instance Arbitrary NetAddr where
  arbitrary = oneof
    [ NetAddrV4   <$> arbitrary <*> arbitrary
    , NetAddrV6   <$> arbitrary <*> arbitrary
    ]
