{ fetchgit, mkDerivation, array, base, bytestring, cborg, cereal, containers
, lens, lens-action, old-time, QuickCheck, quickcheck-instances
, serialise, stdenv, tasty, tasty-quickcheck, template-haskell
, text, time, vector
}:
mkDerivation {
  pname = "safecopy";
  version = "0.10.0.0";
  src = fetchgit {
    url =    "https://github.com/drchaos/safecopy.git";
    rev =    "0eb51dccbbe00cccdb41d800b57d92c1be420989";
    sha256 = "1k286lz1mi2jha93g2kal4b1ql84h5a260nqq9cnqcnbvk44d3y9";
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
