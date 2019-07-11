# Release is pinned to the stable 1903 branch
let
  rev     = "ecc64b374b26df7c198f0d01bd2cb559cd7062a4";
  sha256  = "sha256:1gi76qgf7622hb03yz7kjgw41vy79pfk0gwsvb0m5wq6vi19dp78";
in
import (builtins.fetchTarball {
  inherit sha256;
  name   = "nixos-1809";
  url    = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
})
