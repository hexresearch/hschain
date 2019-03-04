# Release is pinned to the stable 1809 branch
let
#  rev = "e542fc2c94dbebeb51757dbd9ab3f54c41652858";
# sha256 = "1321yr0s7kijpnpzpg3dnqyh92wcmj5d6qpl9jhx7mrvrjniqqkz";
  rev = "889815fd7bf633d3a7c116dbf3598f57129cd221";
  sha256  = "1pxbzasf371gh64174l10nd64bkjlcdn0akpjc3cz25sr0v5vsx9";
in
import (builtins.fetchTarball {
  inherit sha256;
  name   = "nixos-1809";
  url    = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
})
