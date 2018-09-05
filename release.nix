let

  lib = pkgs.haskell.lib;
  config  = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      docker-container = pkgs.dockerTools.buildImage {
        name = "thundermint-node";
        fromImageName = "scratch";
        contents = [haskellPackages.thundermint pkgs.bashInteractive];
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
          lifted-async =  haskellPackagesNew.lifted-async_0_10_0_1;
          async = haskellPackagesNew.async_2_2_1;
          aeson = haskellPackagesNew.aeson_1_3_0_0;
          text = haskellPackagesNew.text_1_2_3_0;
          tasty-hunit = lib.dontCheck haskellPackagesNew.tasty-hunit_0_10_0_1;
          tasty = lib.dontCheck haskellPackagesOld.tasty;
          serialise = lib.dontCheck haskellPackagesOld.serialise;
          network = haskellPackagesOld.callPackage ./deps/network.nix {};
          network-simple = haskellPackagesOld.callPackage ./deps/network-simple.nix {};
          exceptions = haskellPackagesOld.callPackage ./deps/exceptions.nix {};
          safe-exceptions = haskellPackagesOld.callPackage ./deps/safe-exceptions.nix {};
          katip = haskellPackagesOld.callPackage ./deps/katip.nix {};
          free = haskellPackagesOld.callPackage ./deps/free.nix {};
          either = haskellPackagesOld.callPackage ./deps/either.nix {};
          concurrent-output = haskellPackagesOld.callPackage ./deps/concurrent-output.nix {};
          hedgehog = haskellPackagesOld.callPackage ./deps/hedgehog.nix {};
          memory = haskellPackagesOld.callPackage ./deps/memory.nix {};
          cryptonite = lib.dontCheck( haskellPackagesOld.callPackage ./deps/cryptonite.nix {} );
          basement = haskellPackagesOld.callPackage ./deps/basement.nix {};
          foundation = haskellPackagesOld.callPackage ./deps/foundation.nix {};
          retry = haskellPackagesOld.callPackage ./deps/retry.nix {};
          thundermint = pkgs.haskell.lib.overrideCabal
              ( pkgs.haskell.lib.justStaticExecutables
                  ( haskellPackagesNew.callPackage ./thundermint.nix { })
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
