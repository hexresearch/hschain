{ mkDerivation, aeson, base, base58-bytestring, bytestring
, cryptonite, deepseq, entropy, memory, serialise, stdenv, tasty
, tasty-hunit, text, safecopy
}:
mkDerivation {
  pname = "thundermint-crypto";
  version = "0.1";
  src = ../thundermint-crypto;
  libraryHaskellDepends = [
    aeson base base58-bytestring bytestring cryptonite deepseq entropy
    memory serialise text safecopy
  ];
  testHaskellDepends = [
    aeson base bytestring serialise tasty tasty-hunit
  ];
  homepage = "https://github.com/hexresearch/thundermint";
  description = "Simple cryptography API for thundermint";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
