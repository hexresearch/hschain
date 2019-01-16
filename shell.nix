let release = import ./release.nix;
    pkgs    = release.pkgs;
in pkgs.haskellPackages.shellFor {
     nativeBuildInputs = with pkgs.haskellPackages; [
       cabal-install
     ];
     packages = _: pkgs.lib.attrValues release.thundermintPackages;
}
