{ mkDerivation, aeson, async, auto-update, base, blaze-builder
, bytestring, containers, criterion, deepseq, directory, either
, filepath, hostname, microlens, microlens-th, monad-control, mtl
, old-locale, quickcheck-instances, regex-tdfa, resourcet
, safe-exceptions, scientific, semigroups, stdenv, stm, string-conv
, tasty, tasty-golden, tasty-hunit, tasty-quickcheck
, template-haskell, text, time, time-locale-compat, transformers
, transformers-base, transformers-compat, unix, unliftio-core
, unordered-containers
}:
mkDerivation {
  pname = "katip";
  version = "0.5.4.0";
  sha256 = "4d5d3de75fc1217112de4e4eb3c4dffad82ec018c7881560837c73f27f9e8484";
  libraryHaskellDepends = [
    aeson async auto-update base bytestring containers either hostname
    microlens microlens-th monad-control mtl old-locale resourcet
    safe-exceptions scientific semigroups stm string-conv
    template-haskell text time transformers transformers-base
    transformers-compat unix unliftio-core unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory microlens
    quickcheck-instances regex-tdfa safe-exceptions stm tasty
    tasty-golden tasty-hunit tasty-quickcheck template-haskell text
    time time-locale-compat unordered-containers
  ];
  benchmarkHaskellDepends = [
    aeson async base blaze-builder criterion deepseq directory filepath
    safe-exceptions text time transformers unix
  ];
  homepage = "https://github.com/Soostone/katip";
  description = "A structured logging framework";
  license = stdenv.lib.licenses.bsd3;
}
