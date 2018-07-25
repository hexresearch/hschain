{ mkDerivation, base, bytestring, bytestring-lexing, cairo, colour
, containers, fetchgit, HUnit, mtl, stdenv, strptime
, template-haskell, time, vcs-revision
}:
mkDerivation {
  pname = "splot";
  version = "0.3.14";
  src = fetchgit {
    url = "https://github.com/Shimuuar/splot.git";
    sha256 = "0k5jcrp01z1gzbf61782m1iqskwpkfmvcff5waalfml7kn0rpp4s";
    rev = "75f243cc1919de003a95728f1cc12b1daa969195";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring bytestring-lexing cairo colour containers HUnit mtl
    strptime template-haskell time vcs-revision
  ];
  homepage = "http://www.haskell.org/haskellwiki/Splot";
  description = "A tool for visualizing the lifecycle of many concurrent multi-staged processes";
  license = stdenv.lib.licenses.bsd3;
}
