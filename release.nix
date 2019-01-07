let

  lib = pkgs.haskell.lib;
  config  = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      docker-container = pkgs.dockerTools.buildImage {
        name = "thundermint-node";
        fromImageName = "scratch";
        contents = [haskellPackages.thundermint];
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
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          aeson = haskellPackagesNew.aeson_1_3_0_0;
          async = haskellPackagesNew.async_2_2_1;
          basement = haskellPackagesOld.callPackage ./deps/basement.nix {};
          bloodhound = lib.dontCheck (haskellPackagesOld.callPackage ./deps/bloodhound.nix {});
          concurrent-output = haskellPackagesOld.callPackage ./deps/concurrent-output.nix {};
          cryptonite = lib.dontCheck( haskellPackagesOld.callPackage ./deps/cryptonite.nix {} );
          either = haskellPackagesOld.callPackage ./deps/either.nix {};
          exceptions = haskellPackagesOld.callPackage ./deps/exceptions.nix {};
          errors = haskellPackagesOld.callPackage ./deps/errors.nix {};
          foundation = haskellPackagesOld.callPackage ./deps/foundation.nix {};
          free = haskellPackagesOld.callPackage ./deps/free.nix {};
          hedgehog = haskellPackagesOld.callPackage ./deps/hedgehog.nix {};
          katip = haskellPackagesOld.callPackage ./deps/katip.nix {};
          katip-elasticsearch = lib.dontCheck (haskellPackagesOld.callPackage ./deps/katip-elasticsearch.nix {});
          lifted-async =  haskellPackagesNew.lifted-async_0_10_0_1;
          microlens-aeson = haskellPackagesOld.callPackage ./deps/microlens-aeson.nix {};
          memory = haskellPackagesOld.callPackage ./deps/memory.nix {};
          network = haskellPackagesOld.callPackage ./deps/network.nix {};
          network-info = haskellPackagesOld.callPackage ./deps/network-info.nix {};
          network-simple = haskellPackagesOld.callPackage ./deps/network-simple.nix {};
          pipes = haskellPackagesOld.callPackage ./deps/pipes.nix {};
          retry = haskellPackagesOld.callPackage ./deps/retry.nix {};
          safe-exceptions = haskellPackagesOld.callPackage ./deps/safe-exceptions.nix {};
          serialise = lib.dontCheck haskellPackagesOld.serialise;
          tasty = lib.dontCheck haskellPackagesOld.tasty;
          tasty-hunit = lib.dontCheck haskellPackagesNew.tasty-hunit_0_10_0_1;
          text = haskellPackagesNew.text_1_2_3_0;
          entropy = pkgs.haskellPackages.entropy_0_4_1_1;
          safecopy = haskellPackagesOld.callPackage ./deps/safecopy.nix {};
          thundermint-crypto = haskellPackagesOld.callPackage ./deps/thundermint-crypto.nix {};
          thundermint = pkgs.haskell.lib.overrideCabal
              ( pkgs.haskell.lib.justStaticExecutables
                  ( lib.dontCheck(haskellPackagesNew.callPackage ./deps/thundermint.nix {}))
              )( oldDerivation: { }
              );
        };
      };
    };
  };

  pkgs = import ./pkgs.nix { inherit config; };


  callPackage = pkgs.haskellPackages.callPackage;

  self = rec {
    inherit pkgs;
    thundermint = pkgs.haskellPackages.thundermint;
    docker-container = pkgs.docker-container;
    };
in self
