{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- This module contains newtypes which allow easily define 'FromJSON'
-- instances for haskell record which could be used parsing configs
-- either using @aeson@ or @yaml@ libraries. Main focus of this
-- library is to provide instances with good error messages and being
-- easy to write. Performance is secondary priority since config is
-- parsed only once.
module HSChain.Config
  ( Config(..)
  , DropSmart(..)
  , DropN(..)
  , DropPrefix(..)
  , SnakeCase(..)
  , CaseInsensitive(..)
  , WithDefault(..)
  , TopConfig(..)
  ) where

import HSChain.Config.Internal.Impl
