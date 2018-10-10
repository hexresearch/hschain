{ mkDerivation, aeson, attoparsec, base, bytestring, criterion
, deepseq, hashable, lens, lens-aeson, microlens, scientific
, stdenv, tasty, tasty-hunit, text, unordered-containers, vector
}:
mkDerivation {
  pname = "microlens-aeson";
  version = "2.3.0";
  sha256 = "f2f28288bfc190127423a452514d35f7b66f9d5625cf6653bb34cb020aa450c5";
  revision = "2";
  editedCabalFile = "1ri98vr3bbx0l9b4vpmcwhf8fm5lgj92kw4g0v3jx6xajwwc5dc8";
  libraryHaskellDepends = [
    aeson attoparsec base bytestring deepseq hashable microlens
    scientific text unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring deepseq hashable microlens tasty tasty-hunit
    text unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    aeson base bytestring criterion deepseq hashable lens lens-aeson
    microlens text unordered-containers vector
  ];
  homepage = "http://github.com/fosskers/microlens-aeson/";
  description = "Law-abiding lenses for Aeson, using microlens";
  license = stdenv.lib.licenses.mit;
}
