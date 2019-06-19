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
    ];
    buildInputs = [
      pkgs.libsodium
    ];
    packages = release."${ghc}";
    
  }
