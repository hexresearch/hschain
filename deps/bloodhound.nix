{ mkDerivation, aeson, base, blaze-builder, bytestring, containers
, errors, exceptions, hashable, hspec, http-client, http-types
, microlens, microlens-aeson, mtl, network-uri, pretty-simple
, QuickCheck, quickcheck-arbitrary-template, quickcheck-properties
, scientific, semigroups, semver, stdenv, temporary, text, time
, transformers, unix-compat, unordered-containers, vector
}:
mkDerivation {
  pname = "bloodhound";
  version = "0.16.0.0";
  sha256 = "1f3fa9b75b1bef3a5712d35d177ead2e857b727e8af398c6cfeef18ac1139d3c";
  libraryHaskellDepends = [
    aeson base blaze-builder bytestring containers exceptions hashable
    http-client http-types mtl network-uri scientific semigroups semver
    text time transformers unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring containers errors exceptions hspec
    http-client http-types microlens microlens-aeson mtl network-uri
    pretty-simple QuickCheck quickcheck-arbitrary-template
    quickcheck-properties semigroups semver temporary text time
    unix-compat unordered-containers vector
  ];
  homepage = "https://github.com/bitemyapp/bloodhound";
  description = "Elasticsearch client library for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
