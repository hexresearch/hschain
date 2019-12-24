{ isProd ? false
, isProfile ? false
, containerTag ? "latest"
, prefixName ? ""
, ghc        ? "ghc865"
, ...}:
let
  release = import ./release.nix { inherit
    isProd
    isProfile; };
  pkgs = release.pkgs;
  lib = pkgs.haskell.lib;

  hschain-exe = lib.overrideCabal
    (lib.justStaticExecutables (lib.dontCheck pkgs.haskell.packages."${ghc}".hschain-examples)) (oldDerivation: { });

  hschain-docker = pkgs.dockerTools.buildImage {
    name = "${prefixName}hschain";
    fromImageName = "scratch";
    tag = containerTag;
    created = "now";
    contents = [hschain-exe];
    config = {};
  };
in {
  inherit
    hschain-docker;
}
