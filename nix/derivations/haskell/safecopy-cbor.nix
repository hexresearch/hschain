{ mkDerivation, fetchgitPrivate, array, base, bytestring, cborg, cereal, containers
, lens, lens-action, old-time, QuickCheck, quickcheck-instances
, serialise, stdenv, tasty, tasty-quickcheck, template-haskell
, text, time, vector
}:
mkDerivation {
  pname = "safecopy-cbor";
  version = "0.10.0.0";
  src = fetchgitPrivate {
    url    = "git@github.com:hexresearch/safecopy-cbor.git";
    rev    = "5c94d158fb0ddf064d25f6e065ed00b1cb1e1a97";
    sha256 = "1qsyq1hn0zxcmhl98qksqzxanc35k9vjjm7jycq6d8b3dmg6ch7k";
  };
  libraryHaskellDepends = [
    array base bytestring cborg containers old-time serialise
    template-haskell text time vector
  ];
  testHaskellDepends = [
    array base cborg cereal containers lens lens-action QuickCheck
    quickcheck-instances tasty tasty-quickcheck template-haskell time
    vector
  ];
  homepage = "https://github.com/acid-state/safecopy";
  description = "Binary serialization with version control";
  license = stdenv.lib.licenses.publicDomain;
}
