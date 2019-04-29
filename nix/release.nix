{ isProfile ? false
, isProd    ? false
, ...
}:
let
  pkgs   = import ./pkgs.nix { inherit config; overlays=[]; };
  config = {
    allowUnfree = true;
    packageOverrides = super: {
      haskell = haskTools.interpret pkgs super {
        overrides = import ./overrides.nix;
        release   = thundermintPackages;
      };
    };
  };
  # General utilds
  lib       = pkgs.haskell.lib;
  haskTools = import (pkgs.fetchFromGitHub {
    owner  = "hexresearch";
    repo   = "haskell-nix-tools";
    rev    = "65c493f1990829adfdc212affd993ae277663749";
    sha256 = "178fa5s1axqpjfdas3q1plfvvk7ajravw5fiz35c5j5h7bdnl48y";
  }) pkgs;
  hask = haskTools.hask;
  doIf = haskTools.doIf;
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
    hask.doPedantic (lib.doCheck drv));
  profileOverride = doIf isProfile hask.doProfile;
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
    inherit pkgs;
    ghc844 = p: with p; [ thundermint-crypto thundermint-types thundermint ];
    ghc863 = p: with p; [ thundermint-crypto thundermint-types thundermint ];
    ghcjs  = p: with p; [ thundermint-crypto thundermint-types  ];
    };
in release
