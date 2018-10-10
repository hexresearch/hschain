{ mkDerivation, aeson, async, base, base58-bytestring, bloodhound
, bytestring, bytestring-arbitrary, containers, criterion
, cryptonite, data-default-class, deepseq, directory, exceptions
, filepath, generic-arbitrary, hedgehog, hedgehog-quickcheck
, http-client-tls, katip, katip-elasticsearch, memory, network
, network-info, network-simple, optparse-applicative, pipes
, QuickCheck, random, random-shuffle, retry, serialise
, sqlite-simple, stdenv, stm, tasty, tasty-hedgehog, tasty-hunit
, text, thundermint-crypto, time, tls, transformers
, unordered-containers, x509, x509-store, x509-system
, x509-validation
}:
mkDerivation {
  pname = "thundermint";
  version = "0.1";
  src = ../thundermint;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base base58-bytestring bloodhound bytestring containers
    cryptonite data-default-class deepseq directory exceptions filepath
    http-client-tls katip katip-elasticsearch memory network
    network-info pipes random random-shuffle retry serialise
    sqlite-simple stm text thundermint-crypto time tls transformers
    unordered-containers x509 x509-store x509-system x509-validation
  ];
  executableHaskellDepends = [
    aeson async base base58-bytestring bytestring containers
    data-default-class directory exceptions filepath katip network
    network-simple optparse-applicative random serialise stm text
    thundermint-crypto
  ];
  testHaskellDepends = [
    aeson async base bytestring bytestring-arbitrary containers
    data-default-class directory exceptions generic-arbitrary hedgehog
    hedgehog-quickcheck katip network QuickCheck random retry serialise
    stm tasty tasty-hedgehog tasty-hunit thundermint-crypto tls
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion cryptonite deepseq
  ];
  homepage = "https://github.com/hexresearch/thundermint";
  description = "Haskell reimplementation of tendermint protocol";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
