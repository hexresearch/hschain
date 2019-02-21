{ isProd      ? false
, isProfile   ? false
, isGhc86     ? false
, ...
}:
with import ./lib/utils.nix;
let
  lib = pkgs.haskell.lib;
  overrideCabal = lib.overrideCabal;
  justStaticExecutables = lib.justStaticExecutables;
  dontHaddock = lib.dontHaddock;
  doCheck = lib.doCheck;
  doPendantic = drv: overrideCabal drv (drv: {
    buildFlags = (drv.buildFlags or []) ++ ["--ghc-option=-Wall" "--ghc-option=-Werror"];
  });
  doShow = drv: overrideCabal drv (drv: {preCheck = ((drv.preCheck or "") + ''find .. -name "*stm.json"'');});
  enableLibraryProfiling = lib.enableLibraryProfiling;
  enableExecutableProfiling = lib.enableExecutableProfiling;
  prodOverrideE = drv: if isProd then justStaticExecutables (dontHaddock drv) else drv;
  prodOverrideAll = drv: if isProd then doPendantic (doCheck drv) else drv;
  profileOverride = drv: if isProfile then enableExecutableProfiling (enableLibraryProfiling drv) else drv;
  addLibrary = drv: x: addLibraries drv [x];
  addLibraries = drv: xs: overrideCabal drv (drv: { libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ xs; });
  increaseGhcStack = drv: overrideCabal drv (drv: let
    flags = ["--ghcjs-option=+RTS" "--ghcjs-option=-K512M" "--ghcjs-option=-RTS"];
    in {
      buildFlags = (drv.buildFlags or []) ++ flags;
    });
  gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "siers";
    repo = "nix-gitignore";
    rev = "ce0778ddd8b1f5f92d26480c21706b51b1af9166";
    sha256 = "1d7ab78i2k13lffskb23x8b5h24x7wkdmpvmria1v3wb9pcpkg2w";
  }) {};
  ignoreStack = source: let
    ignore-list = ''
      /.stack-work
    '';
    in gitignore.gitignoreSourceAux ignore-list source;

  # Internal packages (depends on production or dev environment)
  callInternal = name: path: args: (prodOverrideAll (profileOverride (
      doShow (dontHaddock ( pkgs.haskellPackages.callCabal2nix name (ignoreStack path) args )
    ))));

  ghcjsOverrides = hsNew: hsOld: {
    SHA                   = lib.dontCheck hsOld.SHA;
    aeson                 = lib.dontCheck hsOld.aeson;
    half                  = lib.dontCheck hsOld.half;
    quickcheck-assertions = lib.dontCheck hsOld.quickcheck-assertions;
    scientific            = lib.dontCheck hsOld.scientific;
    tasty-quickcheck      = lib.dontCheck hsOld.tasty-quickcheck;
    thundermint-crypto    = increaseGhcStack (dontHaddock (addLibrary (callInternal "thundermint" ../thundermint-crypto {}) hsNew.SHA));
  };

  haskOverrides = haskellPackagesNew: haskellPackagesOld: let
          # Overrides from cabal2nix files
          derivationsOverrides = lib.packagesFromDirectory { directory = derivationsPath; } haskellPackagesNew haskellPackagesOld;

          internal = {
            thundermint-types = callInternal "thundermint" ../thundermint-types { };
            thundermint-crypto = callInternal "thundermint" ../thundermint-crypto { };
            thundermint = callInternal "thundermint" ../thundermint { };
            safecopy-cbor = haskellPackagesNew.callPackage derivations/safecopy-cbor.nix {};
          };

          in derivationsOverrides // internal // {
            # Overrides from nixpkgs
            katip-elasticsearch = lib.dontCheck derivationsOverrides.katip-elasticsearch;
            serialise = lib.dontCheck haskellPackagesOld.serialise;
            tasty = lib.dontCheck haskellPackagesOld.tasty;
            bloodhound = if isGhc86 then lib.dontCheck derivationsOverrides.bloodhound else haskellPackagesOld.bloodhound;
          };
  config  = {
    allowUnfree = true;
    packageOverrides = rpkgs: rec {
      haskell = rpkgs.haskell // {
        packageOverrides = haskOverrides;
        packages = rpkgs.haskell.packages // {
          ghcjs = rpkgs.haskell.packages.ghcjs.override {
            overrides = hsNew: hsOld: haskOverrides hsNew hsOld // ghcjsOverrides hsNew hsOld;
          };
        };
      };
    };
  };

  derivationsPath = if isGhc86 then ./derivations/ghc86 else ./derivations;
  pkgsPath = if isGhc86 then  ./pkgs/ghc863.nix else ./pkgs/ghc844.nix;
  pkgs = import pkgsPath { inherit config; overlays=[]; };
  /* pkgs = if isProd then rawPkgs.pkgsMusl else rawPkgs; */

  callPackage = pkgs.haskellPackages.callPackage;

  self = rec {
    inherit pkgs;
    thundermintPackages = {
      inherit (pkgs.haskellPackages)
        thundermint-types
        thundermint-crypto
        thundermint;
      };
    packagesGHCJS = {
      inherit (pkgs.haskell.packages.ghcjs)
      thundermint-crypto
      thundermint-types;
      };
    };
in self
