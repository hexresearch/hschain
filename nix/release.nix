let
  pkgs   = import ./pkgs.nix { inherit config; };
  lib    = pkgs.haskell.lib;
  # Overrides for haskell packages
  haskOverrides = hsNew: hsOld: rec {
    cborg               = hsOld.callPackage ./derivations/cborg.nix {};
    concurrent-output   = hsOld.callPackage ./derivations/concurrent-output.nix {};
    hedgehog            = hsOld.callPackage ./derivations/hedgehog.nix {};
    katip               = hsOld.callPackage ./derivations/katip.nix {};
    katip-elasticsearch = lib.dontCheck (hsOld.callPackage ./derivations/katip-elasticsearch.nix {});
    prometheus-client   = hsOld.callPackage ./derivations/prometheus-client.nix {};
    stm                 = hsOld.callPackage ./derivations/stm.nix {};
    unliftio            = hsOld.callPackage ./derivations/unliftio.nix {};
    # -
    thundermint-crypto  = hsOld.callPackage ./derivations/thundermint-crypto.nix {};
    thundermint-types   = hsOld.callPackage ./derivations/thundermint-types.nix {};
    thundermint         = hsOld.callPackage ./derivations/thundermint.nix {};
    thundermint-exe     = pkgs.haskell.lib.overrideCabal
      (lib.justStaticExecutables (lib.dontCheck thundermint)) (oldDerivation: { });
  };
  # Additional overrides for GHCJS
  ghcjsOverrides = hsNew: hsOld: {
    half                  = lib.dontCheck hsOld.half;
    quickcheck-assertions = lib.dontCheck hsOld.quickcheck-assertions;
    SHA                   = lib.dontCheck hsOld.SHA;
    thundermint-crypto    = hsOld.callPackage ./derivations/thundermint-crypto.nix {isGHCJS = true;};
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
        packages = pkgs.haskell.packages // {
          ghcjs = pkgs.haskell.packages.ghcjs.override {
            overrides = hsNew: hsOld: haskOverrides hsNew hsOld // ghcjsOverrides hsNew hsOld;
          };
        };
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
