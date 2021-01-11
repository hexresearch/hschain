ghc:
let
  rev     = "ffce5a082b464f0715ac4028d7bc79171f20203e";
  sha256  = "sha256:11ffffrbw50nc0gi99p03d3qhcwail05h55f0wk54kbrviaa53jw";
in
import (builtins.fetchTarball {
  inherit sha256;
  name   = "nixos-2009";
  url    = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
})
