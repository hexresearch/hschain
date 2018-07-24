# Nix expression for starting jupyter notebook
let
  pkgs = import <nixpkgs> {};
  pyp  =  pkgs.python36.withPackages (ps: with ps;
    [ jupyter_core
      notebook
      matplotlib
      numpy
      pandas
      statsmodels
    ]);
in
  pkgs.stdenv.mkDerivation {
    name        = "shell";
    buildInputs = [pyp];
    }
