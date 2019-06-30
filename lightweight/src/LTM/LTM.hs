-- |Implementing thundermint as parsing over
-- stream of events, hence "Lightweight ThunderMint".
--

{-# LANGUAGE DeriveAnyClass, DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module LTM.LTM where

import LTM.SP

-- |Type of LTM processor.
--
-- We employ the Fudgets routing approach here.
-- The messages can come from upper level (controller)
-- and lower level (network). They also can come to upper level
-- (we have a new state, look!) and to lower level (send this to that).
type LTM imsg omsg tx state = SP -- the processor
                              (UpDown tx imsg) -- transactions fall from higher level
                                               -- input messages bubble up from lower level
                              (UpDown state omsg) -- state goes to upper level
                                                  -- output messages - to lower level.

