{ isProfile  ? false
, isProd     ? false
, isCoreLint ? false
, isBench    ? true
, useSodium  ? true
, ghc        ? "ghc844"
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
    ];
    packages = release."${ghc}";
    # NOTE: this is workaround for problem with building
    #       hschain-types. Without this build fails
    #       mysteriously. Note that build with nix-build is not
    #       affected.
    shellHook = ''
      export LD_LIBRARY_PATH=${pkgs.libsodium}/lib''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH
      '';
  }
