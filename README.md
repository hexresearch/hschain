# Thundermint

[![Build Status](https://drone1.hxr.team/api/badges/hexresearch/thundermint/status.svg)](https://drone1.hxr.team/hexresearch/thundermint)

Thundermint is a haskell implementation of tendermint bysantine consesus algorithm

## TLS

[certificate generation](./docs/TLS.md)

# Build

## Build with `nix`

Install Nix
```
https://nixos.wiki/wiki/Nix_Installation_Guide
```

Just use following command
```
nix-build -A thundermint release.nix
```
Note that if project directory contains `.ghc.environment.*` files created by new-style cabal commands build will fail.

## Run dev environment with `nix`

```
nix-shell
```

You can use `cabal build` inside opened shell.

To install needed programs just use:

```
nix-env -i cabal-install
```

or

```
nix-env -i hlint
```


Then you can use `cabal` to build thundermit inside this shell or run `thundermint-simple` binary.

## Build with `stack`

```
stack build
```

# Run testing network using terraform

First of all you have to install terraform and docker. Dowload `terraform` from [here](https://www.terraform.io/downloads.html), unzip and put in $PATH.

`terraform` runs nodes in separate docker containers. So, you need to build docker image.

## Build docker image

```
nix-build release.nix -A docker-container
```

All info about building docker images by `nix` I found [here](https://github.com/Gabriel439/haskell-nix/blob/master/project3/README.md#minimizing-the-closure).

When that command is finished you will have `result` simlink that refers to the docker image in tar archive format.

Then you need to load this archive into local docker image.

```
docker load < result
```

## Run testing network

When you run `terraform` at the fisrt time you should run `terraform init` once.

```
terraform apply
```

This command creates all containers and volumes, runs them, and bootstraps cluster with actual ip
addresses.

To list runnig containers use `docker container ls` .

All logs can be found in `logs` volume. Use `docker volume inspect logs` to found directory with
logs.
