# Nix expression for starting jupyter notebook
let
  pkgs   = import <nixpkgs> {inherit config;};
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld:
          rec {
            splot = haskellPackagesOld.callPackage ./nix/splot.nix {};
          };
      };
    };
  };
  # Python packages
  pyp = pkgs.python36.withPackages (ps: with ps;
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
    buildInputs = [
      pyp
      pkgs.haskellPackages.splot
      ];
    #
    FONTCONFIG_FILE="${pkgs.fontconfig.out}/etc/fonts/fonts.conf";
    }
