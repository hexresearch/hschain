{ mkDerivation, async, base, base16-bytestring, base58-bytestring
, bytestring , containers, criterion, cryptonite, deepseq, exceptions, groom
, katip, network , random, serialise, stdenv, stm, sqlite-simple, tasty
, tasty-hunit , time, transformers
}:
mkDerivation {
  pname = "thundermint";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base58-bytestring bytestring containers
    exceptions groom katip network random serialise stm sqlite-simple transformers
  ];
  executableHaskellDepends = [
    base bytestring containers cryptonite
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
