{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "network-info";
  version = "0.2.0.10";
  sha256 = "5680f6975d34cf4f81fa7ca0c8efd682261d6a1119e06dece0f67c7bd97fd52a";
  libraryHaskellDepends = [ base ];
  homepage = "http://github.com/jystic/network-info";
  description = "Access the local computer's basic network configuration";
  license = stdenv.lib.licenses.bsd3;
}
