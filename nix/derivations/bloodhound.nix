{ mkDerivation, aeson, base, blaze-builder, bytestring, containers
, errors, exceptions, hashable, hspec, http-client, http-types
, microlens, microlens-aeson, mtl, network-uri, pretty-simple
, QuickCheck, quickcheck-arbitrary-template, quickcheck-properties
, scientific, semigroups, semver, stdenv, temporary, text, time
, transformers, unix-compat, unordered-containers, vector, fetchgitPrivate
}:
mkDerivation {
  pname = "bloodhound";
  version = "0.16.0.0";
  src = fetchgitPrivate { url = "https://github.com/hexresearch/bloodhound";
                          rev = "2e0bfed96e571c1b7dd70eda36957377844d12a4";
                          sha256 = "0cw6jhq7wngga1hnlbvwdsd97incrgrm0hd5sd03nn773m0a75mb";
                        };
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
