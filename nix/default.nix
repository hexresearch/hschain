args@{
  isProfile ? false
, isProd    ? false
, ghc       ? "ghc844"
}:
let
  release = import ./release.nix args;
  pkgs    = release.pkgs;
in
pkgs.buildEnv {
  name  = "thundermint";
  paths = release."${ghc}" pkgs.haskell.packages."${ghc}";
}
