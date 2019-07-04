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

  baseImage = pkgs.dockerTools.pullImage {
      imageName = "alpine";
      imageDigest = "sha256:e1871801d30885a610511c867de0d6baca7ed4e6a2573d506bbec7fd3b03873f";
      sha256 = "05wcg38vsygjzf59cspfbb7cq98c7x18kz2yym6rbdgx960a0kyq";
  };

  thundermint-docker = pkgs.dockerTools.buildImage {
    name = "${prefixName}thundermint-node";
    fromImage = baseImage;
    tag = containerTag;
    created = "now";
    contents = [thundermint-exe];
  };
in {
  inherit
    thundermint-docker;
}
