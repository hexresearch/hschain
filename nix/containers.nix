{ isProd ? false
, isProfile ? false
, containerTag ? "latest"
, prefixName ? ""
, ...}:
let
  release = import ./release.nix { inherit
    isProd
    isProfile; };
  pkgs = release.pkgs;
  lib = pkgs.haskell.lib;

  thundermint-exe     = lib.overrideCabal
    (lib.justStaticExecutables (lib.dontCheck pkgs.haskellPackages.thundermint)) (oldDerivation: { });

  thundermint-docker = pkgs.dockerTools.buildImage {
    name = "${prefixName}thundermint-node";
    fromImageName = "scratch";
    tag = containerTag;
    contents = [thundermint-exe];
    config = {
      Volumes = {
        "/thundermint" = {};
      };
      ExposedPorts = {
        "49999" = {};
        "50000" = {};
      };
      };
  };
in {
  inherit
    thundermint-docker;
}
