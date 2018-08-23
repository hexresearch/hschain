{ mkDerivation, aeson, async, base, base16-bytestring, base58-bytestring
, bytestring , containers, criterion, cryptonite, deepseq, directory, data-default-class
, exceptions , filepath, groom, katip, network, optparse-applicative, random, retry, serialise, stdenv, stm
, sqlite-simple, tasty , tasty-hunit, text, time, transformers
}:
mkDerivation {
  pname = "thundermint";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base58-bytestring bytestring containers exceptions directory
    data-default-class filepath groom katip network optparse-applicative random
    retry serialise stm sqlite-simple transformers text tasty tasty-hunit
  ];
  executableHaskellDepends = [
    aeson base bytestring containers cryptonite directory filepath text
  ];
  testHaskellDepends = [
    async base base58-bytestring bytestring stm tasty tasty-hunit
  ];
  benchmarkHaskellDepends = [ base bytestring criterion cryptonite deepseq ];
  homepage = "https://github.com/hexresearch/thundermint";
  description = "Haskell reimplementation of tendermint protocol";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
