args@{
  isProfile ? false
, isProd    ? false
, ghc       ? "ghc844"
}:
let
  release = import ./release2.nix args;
  pkgs    = release.pkgs;
in
  pkgs.haskell.packages."${ghc}".shellFor {
    nativeBuildInputs = with pkgs.haskellPackages; [
      cabal-install
    ];
    packages = release."${ghc}";
  }
