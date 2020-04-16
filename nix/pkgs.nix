# Release is pinned to the stable 1903 branch
let
  rev     = "342eaba9ebbc68923c1028a56be1c94e74862832";
  sha256  = "sha256:1wsyk2b73h2f9ljmygqsjqmxxcb1gvs3q6km50y551760na14rqq";
in
import (builtins.fetchTarball {
  inherit sha256;
  name   = "nixos-unstable";
  url    = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
})
