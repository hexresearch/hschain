# Tool for comparing overrides in derivations with ones in the
# in nix.
let
  pkgs = import ./pkgs.nix { config={}; overlays=[]; };
  lib  = pkgs.haskell.lib;
  # Read custom packages from overrides 
  overrides = lib.packagesFromDirectory {
    directory = ./derivations/haskell;
  };
  compare = k: v:
  let nix-version = pkgs.haskellPackages."${k}".version;
  in
  {
      inherit (v) version name;
      inherit nix-version;
      newer = builtins.compareVersions v.version nix-version;
  };
in
builtins.toJSON (builtins.mapAttrs compare (overrides pkgs.haskellPackages {}))
