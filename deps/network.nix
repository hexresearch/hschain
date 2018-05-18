{ mkDerivation, base, bytestring, directory, doctest, hspec, HUnit
, stdenv, unix
}:
mkDerivation {
  pname = "network";
  version = "2.7.0.0";
  sha256 = "c10546f4846f53a0b4b65538bc4fd5db6d4b5d3ac69447e6114be0be0f1a0d9f";
  libraryHaskellDepends = [ base bytestring unix ];
  testHaskellDepends = [
    base bytestring directory doctest hspec HUnit
  ];
  homepage = "https://github.com/haskell/network";
  description = "Low-level networking interface";
  license = stdenv.lib.licenses.bsd3;
}
