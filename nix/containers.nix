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

  hschain-exe     = lib.overrideCabal
    (lib.justStaticExecutables (lib.dontCheck pkgs.haskell.packages."${ghc}".hschain)) (oldDerivation: { });

  hschain-docker = pkgs.dockerTools.buildImage {
    name = "${prefixName}hschain-node";
    fromImageName = "scratch";
    tag = containerTag;
    created = "now";
    contents = [hschain-exe];
    config = {
       Entrypoint = [
         "hschain-coin-node"
       ];
    };
  };
in {
  inherit
    hschain-docker;
}
