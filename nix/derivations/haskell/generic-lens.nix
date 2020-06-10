{ mkDerivation, base, doctest, generic-lens-core, HUnit
, inspection-testing, lens, profunctors, stdenv, text
}:
mkDerivation {
  pname = "generic-lens";
  version = "2.0.0.0";
  sha256 = "b816e55dbdf96ed04b7ad01193fce3dec36397aff3036e34a89ee2884b02093a";
  libraryHaskellDepends = [
    base generic-lens-core profunctors text
  ];
  testHaskellDepends = [
    base doctest HUnit inspection-testing lens profunctors
  ];
  homepage = "https://github.com/kcsongor/generic-lens";
  description = "Generically derive traversals, lenses and prisms";
  license = stdenv.lib.licenses.bsd3;
}
