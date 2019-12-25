{ isProfile  ? false
, isProd     ? false
, isCoreLint ? false
, isBench    ? false
, useSodium  ? true
, ghc        ? "ghc865"
}:
let
  release = import ./release.nix {
    inherit isProd isProfile isCoreLint isBench useSodium;
  };
  pkgs    = release.pkgs;
in
pkgs.buildEnv {
  name  = "hschain";
  paths = release."${ghc}" pkgs.haskell.packages."${ghc}";
}
