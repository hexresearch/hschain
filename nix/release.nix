{ isProfile ? false
, isProd    ? false
, ...
}:
let
  pkgs     = import ./pkgs.nix { inherit config overlays; };
  config   = { allowUnfree = true; };
  overlays = [ overlay ];
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
  # ================================================================
  # Overlay for haskell packages
  overlay = self: super: {
    haskell = haskTools.interpret pkgs super {
      overrides = import ./overrides.nix;
      release   = thundermintPackages;
    };
  };
  # Generate packages for thundermint
  thundermintPackages = hsPkgs: {
    thundermint-crypto = callInternal hsPkgs "thundermint" ../thundermint-crypto {};
    thundermint-types  = callInternal hsPkgs "thundermint" ../thundermint-types  {};
    thundermint-arb    = callInternal hsPkgs "thundermint" ../thundermint-arb    {};
    thundermint        = callInternal hsPkgs "thundermint" ../thundermint        {};
  };
  # Build internal package
  callInternal = hask: name: path: args:
    prodOverride (profileOverride (hask.callCabal2nix name (ignoreStack path) args))
    ;
  prodOverride    = doIf isProd    (drv: hask.doPedantic (lib.doCheck drv));
  profileOverride = doIf isProfile hask.doProfile;
  ignoreStack     = haskTools.ignoreSources ''
    /.stack-work
    '';
  #
  release = {
    inherit pkgs;
    ghc844 = p: with p; [ thundermint-crypto thundermint-types thundermint ];
    ghc863 = p: with p; [ thundermint-crypto thundermint-types thundermint ];
    ghcjs  = p: with p; [ thundermint-crypto thundermint-types  ];
    };
in release
