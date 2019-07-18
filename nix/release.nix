{ isProfile  ? false
, isProd     ? false
, isCoreLint ? false
, isBench    ? false 
, useSodium  ? true
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
      (if useSodium then "-flibsodium" else "-f-libsodium");
    thundermint-quickcheck = callInternal hsPkgs "thundermint" ../thundermint-quickcheck    {} "";
    thundermint-types      = callInternal hsPkgs "thundermint" ../thundermint-types         {} "";
    thundermint            = callInternal hsPkgs "thundermint" ../thundermint               {} "";
    serialise-cddl         = callInternal hsPkgs "thundermint" ../serialise-cddl            {} "";
  };
  # Build internal package
  callInternal = hask: name: path: args: opts:
    benchOverride (lintOverride (prodOverride (profileOverride (hask.callCabal2nixWithOptions name (ignoreStack path) opts args))))
    ;
  lintOverride    = doIf isCoreLint hask.doCoreLint;
  prodOverride    = doIf isProd    (drv: hask.doPedantic (lib.doCheck drv));
  profileOverride = doIf isProfile hask.doProfile;
  benchOverride   = doIf isBench   lib.doBenchmark;
  ignoreStack     = haskTools.ignoreSources ''
    /.stack-work
    '';
  #
  release = let
    thundermintPkgAll = p: with p; [
      serialise-cddl
      thundermint-crypto
      thundermint-types
      thundermint-quickcheck
      thundermint
    ];
    thundermintPkgJs = p: with p; [
      thundermint-crypto
      thundermint-types
    ];
    in
    {
      inherit pkgs;
      ghc844 = thundermintPkgAll;
      ghc865 = thundermintPkgAll;
      ghcjs  = thundermintPkgJs;
    };
in release
