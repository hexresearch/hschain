ghc:
if ghc == "ghcjs" then
  # For GHCJS we use snapshot for which it's known to work since it's
  # broken on master:
  #
  # https://github.com/NixOS/nixpkgs/issues/84563
  let
    rev     = "ecc64b374b26df7c198f0d01bd2cb559cd7062a4";
    sha256  = "sha256:1gi76qgf7622hb03yz7kjgw41vy79pfk0gwsvb0m5wq6vi19dp78";
  in
  import (builtins.fetchTarball {
    inherit sha256;
    name   = "nixos-1809";
    url    = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
  })
else
  # We use master otherwise
  let
    rev     = "342eaba9ebbc68923c1028a56be1c94e74862832";
    sha256  = "sha256:1wsyk2b73h2f9ljmygqsjqmxxcb1gvs3q6km50y551760na14rqq";
  in
  import (builtins.fetchTarball {
    inherit sha256;
    name   = "nixos-unstable";
    url    = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
  })
