{
  description = "HSchain is a collection of packages for blockchain development";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-flake-utils.url = "github:hexresearch/haskell-flake-utils";
    haskell-flake-utils.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake-utils.inputs.flake-utils.follows = "flake-utils";
  };

outputs = { self, nixpkgs, flake-utils, haskell-flake-utils, ... }@inputs:
    haskell-flake-utils.lib.simpleCabalProject2flake {
      inherit self nixpkgs;

      name = "hschain";

      packageNames = [
        "bls-signatures"
        "hschain"
        "hschain-config"
        "hschain-control"
        "hschain-crypto"
        "hschain-crypto-bls"
        "hschain-db"
        "hschain-examples"
        "hschain-examples-types"
        "hschain-logger"
        "hschain-mempool"
        "hschain-merkle"
        "hschain-net"
        "hschain-PoW"
        "hschain-quickcheck"
        "hschain-types"
        "serialise-cddl"
      ];

      # Override haskell packages
      hpPreOverrides = { pkgs }: new: old:
        with pkgs.haskell.lib;
        with haskell-flake-utils.lib;
        tunePackages pkgs old {
          bytestring-arbitrary = [ (jailbreakUnbreak pkgs) ];
          #
          Diff           = [ dontCheck ];
          #
          hschain-crypto = [ dontHaddock ];
          hschain-types  = [ dontHaddock ];
          hschain        = [ dontHaddock ];
        };

      # Arguments for callCabal2nix
      packageCabal2nixArgs = {pkgs}: {
        bls-signatures = {
          bls = pkgs.callPackage (import ./nix/derivations/nix/bls.nix) {};
        };
      };

    };
}
