#!/bin/sh

cabal2nix https://github.com/ucsd-progsys/liquidhaskell.git   > derivations/haskell/liquidhaskell.nix
cabal2nix https://github.com/ucsd-progsys/liquid-fixpoint.git > derivations/haskell/liquid-fixpoint.nix
