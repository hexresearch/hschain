# Tool for comparing overrides in derivations with ones in the
# in nix.
let
  r = import ./release.nix {};
in
builtins.toJSON(
  r.pkgs.haskell.packages.ghc844.local-overrides
)
