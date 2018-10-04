{ mkDerivation, base, basement, bytestring, deepseq, gauge
, ghc-prim, integer-gmp, memory, random, stdenv, tasty, tasty-hunit
, tasty-kat, tasty-quickcheck
}:
mkDerivation {
  pname = "cryptonite";
  version = "0.25";
  sha256 = "89be1a18af8730a7bfe4d718d7d5f6ce858e9df93a411566d15bf992db5a3c8c";
  libraryHaskellDepends = [
    base basement bytestring deepseq ghc-prim integer-gmp memory
  ];
  testHaskellDepends = [
    base bytestring memory tasty tasty-hunit tasty-kat tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    base bytestring deepseq gauge memory random
  ];
  homepage = "https://github.com/haskell-crypto/cryptonite";
  description = "Cryptography Primitives sink";
  license = stdenv.lib.licenses.bsd3;
}
