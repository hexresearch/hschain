{ mkDerivation, base, hpack, QuickCheck, safe, stdenv, tasty
, tasty-golden, tasty-hunit, tasty-quickcheck, template-haskell
, fetchgitPrivate
}:
mkDerivation {
  pname = "quickcheck-arbitrary-template";
  version = "0.2.0.0";
  src = fetchgitPrivate { url = "https://github.com/hexresearch/quickcheck-arbitrary-template.git";
                          rev = "1b32555f2b993cade980f9eac215caa5d77f5763";
                          sha256 = "0fpr179xh0a3h9hp70nnk4z7pb7lizdl0378jkj13wxw2w9h64nk";
                        };
  libraryHaskellDepends = [ base QuickCheck safe template-haskell ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base QuickCheck safe tasty tasty-golden tasty-hunit
    tasty-quickcheck template-haskell
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/plow-technologies/quickcheck-arbitrary-template#readme";
  description = "Generate QuickCheck Gen for Sum Types";
  license = stdenv.lib.licenses.bsd3;
}
