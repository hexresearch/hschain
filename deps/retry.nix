{ mkDerivation, base, data-default-class, exceptions, ghc-prim
, hedgehog, HUnit, mtl, random, stdenv, stm, tasty, tasty-hedgehog
, tasty-hunit, time, transformers
}:
mkDerivation {
  pname = "retry";
  version = "0.7.6.2";
  sha256 = "4d99f0598762de530200477efdba80a51cde2316a698a64f8683b86ba0b8b92e";
  libraryHaskellDepends = [
    base data-default-class exceptions ghc-prim random transformers
  ];
  testHaskellDepends = [
    base data-default-class exceptions ghc-prim hedgehog HUnit mtl
    random stm tasty tasty-hedgehog tasty-hunit time transformers
  ];
  homepage = "http://github.com/Soostone/retry";
  description = "Retry combinators for monadic actions that may fail";
  license = stdenv.lib.licenses.bsd3;
}
