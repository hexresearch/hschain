{ isProfile  ? false
, isProd     ? false
, isCoreLint ? false
, isBench    ? true
, useSodium  ? true
, ghc        ? "ghc865"
}:
let
  release = import ./release.nix {
    inherit isProd isProfile isCoreLint isBench useSodium;
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
    ];
    packages = release."${ghc}";
<<<<<<< HEAD
    # NOTE: this is workaround for bug in GHCi
    #       https://gitlab.haskell.org/ghc/ghc/issues/11042
    #
    #       Without setting LD_LIBRARY_PATH manually GHCi (8.6 and
    #       earlier) cannot load .so and consequently any invocation
    #       of TH fails
    shellHook = ''
      export LD_LIBRARY_PATH=${pkgs.openssl.out}/lib''${LD_LIBRARY_PATH:+:}${pkgs.libsodium}/lib''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH
=======
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
>>>>>>> ade330922d1b9c2c2955c373e6059f566da82b70
      '';
  }
