{ isProfile ? false
, isProd    ? false
, ...
}:
let
  pkgs   = import ./pkgs.nix { inherit config; overlays=[]; };
  config = {
    allowUnfree = true;
    packageOverrides = super: {
      haskell = import ./interpret pkgs super {
        overrides = import ./overrides.nix;
        release   = thundermintPackages;
      };
    };
  };
  # General utilds
  lib  = pkgs.haskell.lib;
  hlib = import ./lib/haskell.nix pkgs;
  util = import ./lib/utils.nix;
  doIf = util.doIf;
  # Generate packages for thundermint
  thundermintPackages = hsPkgs: {
    thundermint-crypto = callInternal hsPkgs "thundermint" ../thundermint-crypto {};
    thundermint-types  = callInternal hsPkgs "thundermint" ../thundermint-types  {};
    thundermint        = callInternal hsPkgs "thundermint" ../thundermint        {};          
  };
  # Build internal package
  callInternal = hask: name: path: args:
    prodOverride (profileOverride (hask.callCabal2nix name (ignoreStack path) args))
    ;
  prodOverride    = doIf isProd (drv:
    hlib.doPedantic (lib.doCheck drv));
  profileOverride = doIf isProfile hlib.doProfile;
  #
  ignoreStack = source:
    let
      gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
        owner = "siers";
        repo = "nix-gitignore";
        rev = "ce0778ddd8b1f5f92d26480c21706b51b1af9166";
        sha256 = "1d7ab78i2k13lffskb23x8b5h24x7wkdmpvmria1v3wb9pcpkg2w";
      }) {};
      ignore-list = ''
        /.stack-work
      '';
    in gitignore.gitignoreSourceAux ignore-list source;
  #
  release = {
            safecopy-cbor = haskellPackagesNew.callPackage derivations/safecopy-cbor.nix {};
    inherit pkgs;
    ghc844 = p: with p; [ thundermint-crypto thundermint-types thundermint ];
    ghc863 = p: with p; [ thundermint-crypto thundermint-types thundermint ];
    ghcjs  = p: with p; [ thundermint-crypto thundermint-types  ];
    };
in release
