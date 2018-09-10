{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "colour";
  version = "1.1.0";
  sha256 = "f24acd891aeea816794485144a7a51e0892229371c9d02209b0b6ac6f15eee47";
  revision = "1";
  editedCabalFile = "1a6jwqw847xvgy1fabq7b26i6klngd2srpb2hsyzyj1wx6n78gv5";
  libraryHaskellDepends = [ base ];
  description = "A model for human colour/color perception";
  license = "unknown";
}
