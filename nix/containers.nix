{ isProd ? false
, isProfile ? false
, containerTag ? "latest"
, prefixName ? ""
, ghc        ? "ghc844"
, ...}:
let
  release = import ./release.nix { inherit
    isProd
    isProfile; };
  pkgs = release.pkgs;
  lib = pkgs.haskell.lib;

  thundermint-exe     = lib.overrideCabal
    (lib.justStaticExecutables (lib.dontCheck pkgs.haskell.packages."${ghc}".thundermint)) (oldDerivation: { });

  thundermint-docker = pkgs.dockerTools.buildImage {
    name = "${prefixName}thundermint-node";
    fromImageName = "scratch";
    tag = containerTag;
    created = "now";
    contents = [thundermint-exe];
    config = {
       Entrypoint = [
         "thundermint-coin-node"
       ];
    };
  };
in {
  inherit
    thundermint-docker;
}
