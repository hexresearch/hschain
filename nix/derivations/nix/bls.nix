{ stdenv, fetchgit, cmake, python }:
stdenv.mkDerivation rec {
  name    = "bls";
  version = "0.1";
  nativeBuildInputs = [ cmake python ];

  # We need to clone submodules as well.
  #
  # deepClone is workaround for that
  src = fetchgit {
    url       = "https://github.com/Chia-Network/bls-signatures";
    deepClone = true;
    rev       = "93e4f4118b326e7f5e2bec1445cc68aac0728026";
    sha256    = "1ns76irr34myr0vzmbw5yw22m7vpg91b1la8s5qg6asngvrdvzik";
  };
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
