# Thundermint 

Thundermint is a haskell implementation of tendermint bysantine consesus algorithm

# Build

## Build with `nix`

```
nix-build -A thundermint release.nix
```

## Run dev environment with `nix`

```
nix-shell
```

Then you can use `cabal` to build thundermit inside this shell or run `thundermint-simple` binary.

## Build with `stack`

```
stack build
```

