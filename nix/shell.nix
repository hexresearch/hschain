{ isProfile  ? false
, isProd     ? false
, isCoreLint ? false
, useSodium  ? false
, ghc        ? "ghc844"
}:
let
  release = import ./release.nix {
    inherit isProd isProfile isCoreLint useSodium;
  };
  pkgs    = release.pkgs;
in
  pkgs.haskell.packages."${ghc}".shellFor {
    nativeBuildInputs = with pkgs.haskellPackages; [
      cabal-install
      pkgs.z3
    ];
    buildInputs = [
      pkgs.libsodium
    ];
    buildInputs =
      if ghc != "ghcjs" then [pkgs.haskell.packages."${ghc}".liquidhaskell] else [];
    packages = release."${ghc}";
  }
