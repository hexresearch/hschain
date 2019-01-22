{ isGHCJS ? false
, mkDerivation, aeson, base, base58-bytestring, bytestring
, cryptonite, deepseq, entropy, memory, serialise, stdenv, tasty
, tasty-hunit, text, SHA, ghcjs-base
}:
mkDerivation {
  pname = "thundermint-crypto";
  version = "0.1";
  src = ../../thundermint-crypto;
  libraryHaskellDepends =
    [ base aeson base58-bytestring bytestring deepseq serialise text ]
    ++ (if isGHCJS
        then [SHA ghcjs-base]
        else [cryptonite entropy memory])
  ;
  testHaskellDepends = [
    aeson base bytestring serialise tasty tasty-hunit text
  ];
  homepage = "https://github.com/hexresearch/thundermint";
  description = "Simple cryptography API for thundermint";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
