{ isProfile  ? false
, isProd     ? false
, isCoreLint ? false
, isBench    ? false
, useSodium
, ghcToUse
, ...
}:
let
  pkgs     = import ./pkgs.nix ghcToUse { inherit config overlays; };
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
    rev    = "b67adafb1f89832a2446fae93ff614c0ccce9ec3";
    sha256 = "sha256:0bxz5gxc5j42hmr7fvxplsnsg2mh8jznl82ca2dw1sspy0vr4pls";
  }) pkgs;
#  haskTools = import ../../haskell-nix-tools pkgs;
  doIf      = haskTools.doIf;
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
    hschain-crypto-bls     = callInternal hsPkgs "hschain" ../hschain-crypto-bls     {} "";
    hschain-quickcheck     = callInternal hsPkgs "hschain" ../hschain-quickcheck     {} "";
    hschain-types          = callInternal hsPkgs "hschain" ../hschain-types          {} "";
    hschain-merkle         = callInternal hsPkgs "hschain" ../hschain-merkle         {} "";
    hschain-mempool        = callInternal hsPkgs "hschain" ../hschain-mempool        {} "";
    hschain-logger         = callInternal hsPkgs "hschain" ../hschain-logger         {} "";
    hschain-db             = callInternal hsPkgs "hschain" ../hschain-db             {} "";
    hschain-net            = callInternal hsPkgs "hschain" ../hschain-net            {} "";
    hschain-config         = callInternal hsPkgs "hschain" ../hschain-config         {} "";
    hschain-pow-func       = callInternal hsPkgs "hschain" ../proof-of-work          {} "";
    hschain-PoW            = callInternal hsPkgs "hschain" ../hschain-PoW            {} "";
    hschain                = callInternal hsPkgs "hschain" ../hschain                {} "";
    hschain-examples       = callInternal hsPkgs "hschain" ../hschain-examples       {} "";
    hschain-examples-types = callInternal hsPkgs "hschain" ../hschain-examples-types {} "";
    hschain-control        = callInternal hsPkgs "hschain" ../hschain-control        {} "";
    serialise-cddl         = callInternal hsPkgs "hschain" ../serialise-cddl         {} "";
  };
  # Build internal package
  callInternal = hask: name: path: args: opts:
    haskTools.compose
      [ haskTools.hask.doFastO2
        (doIf isBench    lib.doBenchmark)
        (doIf isCoreLint haskTools.hask.doCoreLint)
        (doIf isProd    (haskTools.compose
          [ haskTools.hask.doPedantic
            lib.doCheck
          ]))
        (doIf isProfile haskTools.hask.doProfile)
        haskTools.hask.doInstallTests
      ]
      (hask.callCabal2nixWithOptions name (ignoreStack path) opts args);
  ignoreStack     = haskTools.ignoreSources ''
    /.stack-work
    '';
  #
  release = let
    hschainPkgAll = p: with p; [
      serialise-cddl
      bls-signatures
      hschain-crypto
      hschain-crypto-bls
      hschain-types
      hschain-merkle
      hschain-mempool
      hschain-logger
      hschain-config
      hschain-quickcheck
      hschain-control
      hschain-net
      hschain-db
      hschain-pow-func
      hschain-PoW
      hschain
      hschain-examples
      hschain-examples-types
    ];
    hschainPkgJs = p: with p; [
      hschain-crypto
      hschain-control
      hschain-types
      hschain-merkle
      hschain-examples-types
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
