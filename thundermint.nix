{ mkDerivation, aeson, async, base, base16-bytestring, base58-bytestring
, bytestring , bytestring-arbitrary, containers, criterion, cryptonite, deepseq
, directory, data-default-class, exceptions, filepath, generic-arbitrary, groom
, hedgehog, hedgehog-quickcheck, katip, network, QuickCheck, random, retry, serialise
, stdenv, stm, sqlite-simple, tasty, tasty-hunit, tasty-hedgehog, text, time, transformers
}:
mkDerivation {
  pname = "thundermint";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base58-bytestring bytestring containers exceptions directory
    data-default-class filepath groom katip network random retry serialise
    stm sqlite-simple transformers text
  ];
  executableHaskellDepends = [
    aeson base bytestring containers cryptonite directory filepath text
  ];
  testHaskellDepends = [
    async base base58-bytestring bytestring bytestring-arbitrary generic-arbitrary hedgehog
    hedgehog-quickcheck QuickCheck stm tasty tasty-hunit tasty-hedgehog
  ];
  benchmarkHaskellDepends = [ base bytestring criterion cryptonite deepseq ];
  homepage = "https://github.com/hexresearch/thundermint";
  description = "Haskell reimplementation of tendermint protocol";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
