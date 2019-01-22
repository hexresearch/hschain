let
  pkgs   = import ./pkgs.nix { inherit config; };
  lib    = pkgs.haskell.lib;
  haskOverrides = hsNew: hsOld: rec {
    concurrent-output   = hsOld.callPackage ./derivations/concurrent-output.nix {};
    hedgehog            = hsOld.callPackage ./derivations/hedgehog.nix {};
    katip               = hsOld.callPackage ./derivations/katip.nix {};
    katip-elasticsearch = lib.dontCheck (hsOld.callPackage ./derivations/katip-elasticsearch.nix {});
    prometheus-client   = hsOld.callPackage ./derivations/prometheus-client.nix {};
    stm                 = hsOld.callPackage ./derivations/stm.nix {};
    # -
    thundermint-crypto  = hsOld.callPackage ./derivations/thundermint-crypto.nix {};
    thundermint-types   = hsOld.callPackage ./derivations/thundermint-types.nix {};
    thundermint         = hsOld.callPackage ./derivations/thundermint.nix {};
    thundermint-exe     = pkgs.haskell.lib.overrideCabal
      (lib.justStaticExecutables (lib.dontCheck thundermint)) (oldDerivation: { });
  };
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      docker-container = pkgs.dockerTools.buildImage {
        name = "thundermint-node";
        fromImageName = "scratch";
        contents = [pkgs.haskellPackages.thundermint-exe];
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
      haskell = pkgs.haskell // {
        packageOverrides = haskOverrides;
      };
    };
  };
  # ----------------------------------------
  packagesGHC = rec {
    thundermint         = pkgs.haskellPackages.thundermint;
    thundermint-exe     = pkgs.haskellPackages.thundermint-exe;
    docker-container    = pkgs.docker-container;
    thundermintPackages = {
      inherit (pkgs.haskellPackages)
        thundermint
        thundermint-types
        thundermint-crypto
        thundermint-exe
       ;
    };
  };
  packagesGHCJS = {
    inherit (pkgs.haskell.packages.ghcjs)
      thundermint-crypto
    ;
  };
in { inherit pkgs packagesGHC packagesGHCJS; }
