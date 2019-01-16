let

  lib = pkgs.haskell.lib;
  config  = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      docker-container = pkgs.dockerTools.buildImage {
        name = "thundermint-node";
        fromImageName = "scratch";
        contents = [haskellPackages.thundermint-exe];
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
      haskellPackages = pkgs.haskellPackages.override {
        overrides = hsNew: hsOld: rec {
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
              ( pkgs.haskell.lib.justStaticExecutables
                ( lib.dontCheck thundermint))( oldDerivation: { }
              );
        };
      };
    };
  };

  pkgs = import ./pkgs.nix { inherit config; };

  callPackage = pkgs.haskellPackages.callPackage;

  self = rec {
    inherit pkgs;
    thundermint         = pkgs.haskellPackages.thundermint;
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
in self
