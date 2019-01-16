{ mkDerivation, aeson, async, base, base58-bytestring, bytestring
, containers, data-default-class, deepseq, serialise, stdenv, text
, thundermint-crypto, time, unordered-containers
}:
mkDerivation {
  pname = "thundermint-types";
  version = "0.1";
  src = ../../thundermint-types;
  libraryHaskellDepends = [
    aeson async base base58-bytestring bytestring containers
    data-default-class deepseq serialise text thundermint-crypto time
    unordered-containers
  ];
  homepage = "https://github.com/hexresearch/thundermint";
  description = "Core data types of thundermint";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
