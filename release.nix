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
          concurrent-output   = hsOld.callPackage ./deps/concurrent-output.nix {};
          hedgehog            = hsOld.callPackage ./deps/hedgehog.nix {};
          katip               = hsOld.callPackage ./deps/katip.nix {};
          katip-elasticsearch = lib.dontCheck (hsOld.callPackage ./deps/katip-elasticsearch.nix {});
          prometheus-client   = hsOld.callPackage ./deps/prometheus-client.nix {};
          stm                 = hsOld.callPackage ./deps/stm.nix {};
          # -
          thundermint-crypto  = hsOld.callPackage ./deps/thundermint-crypto.nix {};
          thundermint-types   = hsOld.callPackage ./deps/thundermint-types.nix {};
          thundermint         = hsOld.callPackage ./deps/thundermint.nix {};
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
