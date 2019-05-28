{ isProfile  ? false
, isProd     ? false
, isCoreLint ? false
, ghc        ? "ghc844"
}:
let
  release = import ./release.nix {
    inherit isProd isProfile isCoreLint;
  };
  pkgs    = release.pkgs;
in
pkgs.buildEnv {
  name  = "thundermint";
  paths = release."${ghc}" pkgs.haskell.packages."${ghc}";
}
