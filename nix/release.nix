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
    # cabal2nix generates package name as bls so we're going with it
    bls      = self.callPackage (import ./derivations/nix/bls.nix) {};
    bls-wasm = self.callPackage (import ./derivations/nix/bls-wasm.nix) {};
    haskell  = haskTools.interpret pkgs super {
      overrides = import ./overrides.nix;
      release   = hschainPackages;
    };
  };
  # Generate packages for hschain
  hschainPackages = hsPkgs: {
    bls-signatures = callInternal hsPkgs "bls-signatures" ../bls-signatures {} "";
    hschain-crypto = callInternal hsPkgs "hschain" ../hschain-crypto {}
      (if useSodium then "-flibsodium" else "-f-libsodium");
    hschain-crypto-bls = callInternal hsPkgs "hschain" ../hschain-crypto-bls    {} "";
    hschain-quickcheck = callInternal hsPkgs "hschain" ../hschain-quickcheck    {} "";
    hschain-types      = callInternal hsPkgs "hschain" ../hschain-types         {} "";
    hschain-merkle     = callInternal hsPkgs "hschain" ../hschain-merkle        {} "";
    hschain-net        = callInternal hsPkgs "hschain" ../hschain-net           {} "";
    hschain            = callInternal hsPkgs "hschain" ../hschain               {} "";
    hschain-examples   = callInternal hsPkgs "hschain" ../hschain-examples      {} "";
    hschain-control    = callInternal hsPkgs "hschain" ../hschain-control       {} "";
    serialise-cddl     = callInternal hsPkgs "hschain" ../serialise-cddl        {} "";
  };
  # Build internal package
  callInternal = hask: name: path: args: opts:
    doFast (benchOverride (lintOverride (prodOverride (profileOverride (hask.callCabal2nixWithOptions name (ignoreStack path) opts args)))))
    ;
  lintOverride    = doIf isCoreLint hask.doCoreLint;
  prodOverride    = doIf isProd    (drv: hask.doPedantic (lib.doCheck drv));
  profileOverride = doIf isProfile hask.doProfile;
  benchOverride   = doIf isBench   lib.doBenchmark;
  ignoreStack     = haskTools.ignoreSources ''
    /.stack-work
    '';
  doFast = hask.addBuildFlags ["--ghc-option=-O2"];
  #
  release = let
    hschainPkgAll = p: with p; [
      serialise-cddl
      bls-signatures
      hschain-crypto
      hschain-crypto-bls
      hschain-types
      hschain-merkle
      hschain-quickcheck
      hschain-control
      hschain-net
      hschain
      hschain-examples
    ];
    hschainPkgJs = p: with p; [
      hschain-crypto
      hschain-control
      hschain-types
      hschain-merkle
    ];
    in
    {
      inherit pkgs;
      ghc844 = hschainPkgAll;
      ghc865 = hschainPkgAll;
      ghc883 = hschainPkgAll;
      ghcjs  = hschainPkgJs;
    };
in release
