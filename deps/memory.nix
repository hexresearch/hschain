{ mkDerivation, base, basement, bytestring, deepseq, foundation
, ghc-prim, stdenv, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "memory";
  version = "0.14.14";
  sha256 = "1d1b985620155dbacfc9d924b49505889a558f5a7787bf308fad418ded59960e";
  libraryHaskellDepends = [
    base basement bytestring deepseq foundation ghc-prim
  ];
  testHaskellDepends = [
    base basement bytestring foundation tasty tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/vincenthz/hs-memory";
  description = "memory and related abstraction stuff";
  license = stdenv.lib.licenses.bsd3;
}
