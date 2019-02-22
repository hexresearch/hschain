args@{
  isProd      ? false
, isProfile   ? false
}:
let release = import ./release.nix args;
    pkgs = release.pkgs;
in release.pkgs.haskellPackages.shellFor {
    nativeBuildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
    ];
    packages = _: pkgs.lib.attrValues release.thundermintPackages;
  }
