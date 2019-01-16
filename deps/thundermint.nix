{ mkDerivation, aeson, async, base, base58-bytestring, bloodhound
, bytestring, bytestring-arbitrary, containers, criterion
, cryptonite, data-default-class, deepseq, directory, exceptions
, filepath, generic-arbitrary, hedgehog, hedgehog-quickcheck
, http-client-tls, katip, katip-elasticsearch, microlens
, microlens-mtl, network, network-info, network-simple
, optparse-applicative, pipes, prometheus-client
, prometheus-metrics-ghc, QuickCheck, random, random-shuffle, retry
, serialise, sqlite-simple, stdenv, stm, tasty, tasty-hedgehog
, tasty-hunit, tasty-quickcheck, text, thundermint-crypto
, thundermint-types, tls, transformers, unordered-containers
, wai-middleware-prometheus, warp, x509, x509-store, x509-system
, x509-validation
}:
mkDerivation {
  pname = "thundermint";
  version = "0.1";
  src = ../thundermint;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base58-bytestring bloodhound bytestring containers
    data-default-class deepseq directory exceptions filepath
    http-client-tls katip katip-elasticsearch microlens microlens-mtl
    network network-info pipes prometheus-client random random-shuffle
    retry serialise sqlite-simple stm text thundermint-crypto
    thundermint-types tls transformers unordered-containers x509
    x509-store x509-system x509-validation
  ];
  executableHaskellDepends = [
    aeson base bytestring data-default-class exceptions filepath katip
    network network-simple optparse-applicative prometheus-client
    prometheus-metrics-ghc thundermint-crypto thundermint-types
    transformers wai-middleware-prometheus warp
  ];
  testHaskellDepends = [
    aeson async base bytestring bytestring-arbitrary containers
    exceptions generic-arbitrary hedgehog hedgehog-quickcheck microlens
    network QuickCheck random retry serialise sqlite-simple stm tasty
    tasty-hedgehog tasty-hunit tasty-quickcheck thundermint-crypto
    thundermint-types
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion cryptonite deepseq
  ];
  homepage = "https://github.com/hexresearch/thundermint";
  description = "Haskell reimplementation of tendermint protocol";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
