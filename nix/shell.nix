let cfg = import ./config.nix;
in
{ isProfile  ? false
, isProd     ? false
, isCoreLint ? false
, isBench    ? true
, useSodium  ? cfg.useSodium
, ghc        ? cfg.ghc
}:
let
  release = import ./release.nix {
    inherit isProd isProfile isCoreLint isBench useSodium;
    ghcToUse = ghc;
  };
  pkgs    = release.pkgs;
in
  pkgs.haskell.packages."${ghc}".shellFor {
    nativeBuildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
      stylish-haskell
    ];
    buildInputs = [
      pkgs.libsodium
      pkgs.bls
      pkgs.openssl
      pkgs.utillinux
      pkgs.pkg-config
    ];
    packages = release."${ghc}";
    # NOTE: this is workaround for problem with building
    #       hschain-types. Without this build fails
    #       mysteriously. Note that build with nix-build is not
    #       affected.
    #
    # NOTE: We have to set correct CPU affinity manually because
    #       otherwise nix will just pin everything to one core
    #
    #       https://github.com/NixOS/nix/issues/3345
    shellHook = ''
      export LD_LIBRARY_PATH=${pkgs.libsodium}/lib''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH
      taskset -pc 0-1000 $$
      '';
  }
