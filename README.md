# Thundermint 

Thundermint is a haskell implementation of tendermint bysantine consesus algorithm

# Build

## Build with `nix`

Just use following command 
```
nix-build -A thundermint release.nix
```
Note that if project directory contains `.ghc.environment.*` files created by new-style cabal commands build will fail. 

## Run dev environment with `nix`

```
nix-shell
```

Then you can use `cabal` to build thundermit inside this shell or run `thundermint-simple` binary.

## Build with `stack`

```
stack build
```

