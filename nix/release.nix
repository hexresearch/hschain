{ isProfile  ? false
, isProd     ? false
, isCoreLint ? false
, useSodium  ? false
, ...
}:
let
  pkgs     = import ./pkgs.nix { inherit config overlays; };
  config   = {
    allowUnfree = true;
    allowBroken = true;
  };
  overlays = [ overlay ];
  # General utilds
  lib       = pkgs.haskell.lib;
  haskTools = import (pkgs.fetchFromGitHub {
    owner  = "hexresearch";
    repo   = "haskell-nix-tools";
    rev    = "c8749460118960579f992bb74ff01a9c8520e1d7";
    sha256 = "sha256:0fgwckxj0qfia4hik4i4l1gn4vx4cr7329vfxgf65p9bkiz0fyi8";
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
    thundermint-crypto = callInternal hsPkgs "thundermint" ../thundermint-crypto {}
      (if useSodium then "-flibsodium" else "");
    thundermint-quickcheck = callInternal hsPkgs "thundermint" ../thundermint-quickcheck    {} "";
    thundermint-types      = callInternal hsPkgs "thundermint" ../thundermint-types         {} "";
    thundermint            = callInternal hsPkgs "thundermint" ../thundermint               {} "";
  };
  # Build internal package
  callInternal = hask: name: path: args: opts:
    lintOverride (prodOverride (profileOverride (hask.callCabal2nixWithOptions name (ignoreStack path) opts args)))
    ;
  lintOverride    = doIf isCoreLint hask.doCoreLint;
  prodOverride    = doIf isProd    (drv: hask.doPedantic (lib.doCheck drv));
  profileOverride = doIf isProfile hask.doProfile;
  ignoreStack     = haskTools.ignoreSources ''
    /.stack-work
    '';
  #
  release = {
    inherit pkgs;
    ghc844 = p: with p; [ thundermint-crypto thundermint-types thundermint-quickcheck thundermint ];
    ghc865 = p: with p; [ thundermint-crypto thundermint-types thundermint-quickcheck thundermint ];
    ghcjs  = p: with p; [ thundermint-crypto thundermint-types  ];
    };
in release
