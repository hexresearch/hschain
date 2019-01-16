{ mkDerivation, atomic-primops, base, bytestring, clock, containers
, criterion, deepseq, doctest, exceptions, hspec, mtl, QuickCheck
, random, random-shuffle, stdenv, stm, text, transformers
, transformers-compat, utf8-string
}:
mkDerivation {
  pname = "prometheus-client";
  version = "1.0.0";
  sha256 = "279e415720adb45a0ca67af18029c7b9e2ea0a34ec79b7278ed8e20e135b3344";
  libraryHaskellDepends = [
    atomic-primops base bytestring clock containers deepseq exceptions
    mtl stm text transformers transformers-compat utf8-string
  ];
  testHaskellDepends = [
    atomic-primops base bytestring clock containers deepseq doctest
    exceptions hspec mtl QuickCheck random-shuffle stm text
    transformers transformers-compat utf8-string
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion random text utf8-string
  ];
  homepage = "https://github.com/fimad/prometheus-haskell";
  description = "Haskell client library for http://prometheus.io.";
  license = stdenv.lib.licenses.asl20;
}
