let

  lib = pkgs.haskell.lib;
  config  = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          tasty = lib.dontCheck haskellPackagesOld.tasty;
          serialise = lib.dontCheck haskellPackagesOld.serialise;
          tasty-hunit = lib.dontCheck (haskellPackagesOld.callPackage ./deps/tasty-hunit.nix {});
          lifted-async =  haskellPackagesOld.lifted-async_0_10_0_1;
          async = haskellPackagesOld.async_2_2_1;
          network = haskellPackagesOld.callPackage ./deps/network.nix {};
          exceptions = haskellPackagesOld.callPackage ./deps/exceptions.nix {};
          safe-exceptions = haskellPackagesOld.callPackage ./deps/safe-exceptions.nix {};
          katip = haskellPackagesOld.callPackage ./deps/katip.nix {};
          free = haskellPackagesOld.callPackage ./deps/free.nix {};
          either = haskellPackagesOld.callPackage ./deps/either.nix {};
          concurrent-output = haskellPackagesOld.callPackage ./deps/concurrent-output.nix {};
          hedgehog = haskellPackagesOld.callPackage ./deps/hedgehog.nix {};
          memory = haskellPackagesOld.callPackage ./deps/memory.nix {};
          cryptonite = haskellPackagesOld.callPackage ./deps/cryptonite.nix {};
          basement = haskellPackagesOld.callPackage ./deps/basement.nix {};
          foundation = haskellPackagesOld.callPackage ./deps/foundation.nix {};
        };
      };
    };
  };

  pkgs = import ./pkgs.nix { inherit config; };


  callPackage = pkgs.haskellPackages.callPackage;

  self = rec {
    inherit pkgs;
    thundermint = callPackage ./thundermint.nix { };
    };
in self
