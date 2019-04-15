{ mkDerivation, base, containers, mtl, optparse-applicative
, reducers, split, stdenv, stm, tagged, tasty, transformers
}:
mkDerivation {
  pname = "tasty-rerun";
  version = "1.1.14";
  sha256 = "f9d34a3a25e6af36360272e5865f94c2a4b76311908afa25133c3ca21b921f68";
  libraryHaskellDepends = [
    base containers mtl optparse-applicative reducers split stm tagged
    tasty transformers
  ];
  homepage = "http://github.com/ocharles/tasty-rerun";
  description = "Run tests by filtering the test tree depending on the result of previous test runs";
  license = stdenv.lib.licenses.bsd3;
}
