# Nix expression for starting jupyter notebook
let
  pkgs = import <nixpkgs> {inherit config; overlays=[];};
  # Haskell
  haskOverrides = hsPkgNew: hsPkgOld: {
    splot = hsPkgOld.callPackage ./nix/splot.nix {};
  };
  config = {
    packageOverrides = super: {
      haskell = super.haskell // {
        packageOverrides = self: super: haskOverrides self super;
      };
    };
  };
  # Python packages
  pyp = pkgs.python3.withPackages (ps: with ps;
    [ jupyter_core
      notebook
      matplotlib
      numpy
      pandas
      statsmodels
      elasticsearch
      elasticsearch-dsl
    ]);
in
  pkgs.stdenv.mkDerivation {
    name        = "shell";
    buildInputs = [
      pyp
#      pkgs.haskell.packages.ghc844.callPackage ./nix/splot.nix
      pkgs.haskell.packages.ghc844.splot
      ];
    #
    FONTCONFIG_FILE="${pkgs.fontconfig.out}/etc/fonts/fonts.conf";
    }
