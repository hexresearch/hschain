let cfg = import ./config.nix;
in
{ isProfile  ? false
, isProd     ? false
, isCoreLint ? false
, isBench    ? false
, useSodium  ? true
, ghc        ? cfg.ghc
}:
let
  release = import ./release.nix {
    inherit isProd isProfile isCoreLint isBench useSodium;
    ghcToUse = ghc;
  };
  pkgs    = release.pkgs;
in
pkgs.buildEnv {
  name  = "hschain";
  paths = release."${ghc}" pkgs.haskell.packages."${ghc}";
}
