{ mkDerivation, base, criterion, exceptions, mmorph, mtl
, optparse-applicative, QuickCheck, semigroups, stdenv
, test-framework, test-framework-quickcheck2, transformers, void
}:
mkDerivation {
  pname = "pipes";
  version = "4.3.9";
  sha256 = "5c4cda351f9cf59376832baaeb857db25bd4990fd78c4b061aca0bde47271acb";
  libraryHaskellDepends = [
    base exceptions mmorph mtl semigroups transformers void
  ];
  testHaskellDepends = [
    base mtl QuickCheck test-framework test-framework-quickcheck2
    transformers
  ];
  benchmarkHaskellDepends = [
    base criterion mtl optparse-applicative transformers
  ];
  description = "Compositional pipelines";
  license = stdenv.lib.licenses.bsd3;
}
