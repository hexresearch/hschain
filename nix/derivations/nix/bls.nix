{ stdenv, fetchgit, cmake, python, gmp }:
stdenv.mkDerivation rec {
  name    = "bls";
  version = "0.1";
  buildInputs       = [ gmp ];
  nativeBuildInputs = [ cmake python ];

  src = fetchgit {
    url       = "https://github.com/Chia-Network/bls-signatures";
    rev       = "93e4f4118b326e7f5e2bec1445cc68aac0728026";
    sha256    = "13x9rvsfdpjhwrgbzr08c27633dz5gvlgh4974x2r0chrz6q94xd";
  };
  # Patch out linking for GMP. We still build with GMP but don't link
  # it since it causes problems when linking haskell programs
  patches = [
    ./0001-Remove-GMP-linking.patch
    ./0002-Patch-out-building-of-ython-bindings.patch
    ];
  # chiabls/bls.hpp uses:
  #
  # > #include "relic_conf.h"
  #
  # header is howver in the $out/include/relic/ and compiler couldn't
  # locate it. Easist fix is to just move headers.
  postInstall = ''
    mv $out/include/relic/* $out/include/
    rm -rf $out/include/relic
    '';
}
