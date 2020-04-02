# HSchain

[![Build Status](https://drone.hxr.dev/api/badges/hexresearch/hschain/status.svg)]()

HSchain is a collection of packages for blockchain development. Currently only
PBFT-based consensus algorithms is implemented. PoW development is under way.
Following packages could be conidered relatively stable:

 - **hschain-crypto** — wrappers for cryptography which work both on GHC and
   GHCJS.

 - **hschain-merkle** — data structures for Merkle trees.

 - **hschain-control** — various utilities for control flow and concurrency.

 - **hschain-types** — data types for hschain. It's split into separate package
   to make possible to reuse data type in GHCJS.

 - **hschain** — implementation of PBFT-based consensus engine.

 - **hschain-examples** — examples and tests for hschain.

Rest of packages in repository are highly experimental.



# Development

## Nix

Standard way of working on hschain is to use [Nix](https://nixos.org/) to set up
development environment. To use this method one need to [install
Nix](https://nixos.wiki/wiki/Nix_Installation_Guide) unless it's not already
done. After that one only need to type

```
$ nix-shell
```
in project root. After new-style cabal should be used. For example:
```
$ cabal new-build all
```

Note that if project directory contains `.ghc.environment.*` files created by
new-style cabal commands build will fail.

## Cabal

It's possible to use cabal without nix but one'll have to install required C
libraries manually (some configurations require libsodium)

## Stack

It's also possible to use
[stack](https://docs.haskellstack.org/en/stable/README/) for development

```
stack build
```

# Build with `docker`

Docker image https://github.com/phadej/docker-ghc can be used to build project.

Executables are compatable with Ubuntu 18.04.

Steps to build:

* Download docker image: `docker pull phadej/ghc:8.6.5-bionic`
* Login into: `docker run -it --rm --mount type=bind,src=$PWD,target=/project --workdir /project phadej/ghc:8.6.5-bionic /bin/bash --login`
* Now in image:
    * Update cabal db: `cabal new-update`
    * Build exe: `cabal new-build hschain-dioxane-node --flags="-libsodium"`. Note turned off flag `libsodium`, so it is not need to install `libsodium` library in image.
    * Last output of `cabal` will contain path to built executable.


# Build docker image

```
cd nix
build-docker.sh
```

All info about building docker images by `nix` I found
[here](https://github.com/Gabriel439/haskell-nix/blob/master/project3/README.md#minimizing-the-closure).

When that command is finished you will have `result` simlink that refers to the docker image in tar archive format.

`build-docker.sh` loads this archive into local docker image.
