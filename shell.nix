with import ./release.nix;
  #let
  #  nixpkgs = import <nixpkgs> {};
  #in pkgs.stdenv.mkDerivation {
  #  name        = "dev-env";
  #  buildInputs = [pkgs.ghc pkgs.zlib];
  #  system      = builtins.currentSystem;
  #}
  #(pkgs.haskell.lib.doBenchmark thundermint).env 
  thundermint.env 
