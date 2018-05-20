with import ./release.nix;
  let
    nixpkgs = import <nixpkgs> {};
  in pkgs.stdenv.mkDerivation {
    name        = "dev-env";
    buildInputs = [pkgs.ghc thundermint];
    system      = builtins.currentSystem;
  }
