# Nix expression for starting jupyter notebook
let
  pkgs = let rev = "e542fc2c94dbebeb51757dbd9ab3f54c41652858";
             sha256 = "1321yr0s7kijpnpzpg3dnqyh92wcmj5d6qpl9jhx7mrvrjniqqkz";
         in import (builtins.fetchTarball {
              inherit sha256;
              name   = "nixos-1809";
              url    = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
            }) {inherit config; overlays=[];};
  #
  config = {
    packageOverrides = super: {
      python3 = super.python3.override { packageOverrides = pyOverrides;   };
      haskell = super.haskell //       { packageOverrides = haskOverrides; };
    };
  };
  # Haskell overrides
  haskOverrides = hsSelf: hsSuper: {
    splot = hsSelf.callPackage ./nix/splot.nix {};
  };
  # Python overrides
  pyOverrides = pySelf: pySuper: {
    patsy = pySuper.patsy.overridePythonAttrs (_: { checkPhase = ""; });
  };
  # Python packages
  pyp = pkgs.python3.withPackages (ps: with ps;
    [ jupyter_core            
      notebook
      ipywidgets
      matplotlib
      numpy
      pandas
      statsmodels
      elasticsearch
      elasticsearch-dsl
    ]);
  # tex for export of PDFs. PDF export doesn't work in fact. It drops
  # all HTML output and plots. Only things that matter in fact!
  #
  # tex = pkgs.texlive.combine {
  #   inherit (pkgs.texlive)
  #     scheme-medium
  #     adjustbox
  #     collectbox
  #     upquote
  #     ucs
  #     enumitem
  #   ;
  # };
in
  pkgs.mkShell {
    buildInputs = [
      pyp
      pkgs.haskell.packages.ghc844.splot
      pkgs.pandoc
      ];
    #
    FONTCONFIG_FILE="${pkgs.fontconfig.out}/etc/fonts/fonts.conf";
    }
