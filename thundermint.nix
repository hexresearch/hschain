{ mkDerivation, async, base, base16-bytestring, base58-bytestring
, bytestring , containers, cryptonite, exceptions, groom, katip, network
, serialise, stdenv, stm, tasty, tasty-hunit, time, transformers
}:
mkDerivation {
  pname = "thundermint";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base58-bytestring bytestring containers
    exceptions groom katip network serialise stm transformers
  ];
  executableHaskellDepends = [
    base base16-bytestring base58-bytestring bytestring
    containers cryptonite
  ];
  testHaskellDepends = [
    async base bytestring stm tasty tasty-hunit
  ];
  homepage = "https://github.com/hexresearch/thundermint";
  description = "Haskell reimplementation of tendermint protocol";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
