# Various utilities for haskell
pkgs:
let
  lib = pkgs.haskell.lib;
in
rec {
  /* Add flags to GHC invocation to derivations
   */
  addGhcFlags = flags: drv: lib.overrideCabal drv (drv: {
    buildFlags = (drv.buildFlags or []) ++ map (x: "--ghc-option="+x) flags;
  });

  /* Add -Wall and -Werror flags to derivation
   */
  doPedantic = addGhcFlags ["-Wall" "-Werror"];

  /* Add build derivation with profiling enabled
   */
  doProfile = drv: lib.enableExecutableProfiling (lib.enableLibraryProfiling drv);  
}
