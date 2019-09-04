# Thundermint

[![Build Status](https://drone.hxr.dev/api/badges/hexresearch/thundermint/status.svg)](https://drone.hxr.dev/hexresearch/thundermint)

HSchain is a haskell implementation of tendermint bysantine consesus algorithm

## TLS

[certificate generation](./docs/TLS.md)

# Build

Thundermint could be build either with `stack` or with `cabal`. Following
command will build it using stack:

```
stack build
```

Or with cabal:

```
cabal new-build all
```

## Build with `nix`

Install Nix
```
https://nixos.wiki/wiki/Nix_Installation_Guide
```

Just use following command
```
cd nix
build.sh
```
Note that if project directory contains `.ghc.environment.*` files created by new-style cabal commands build will fail.

## Run dev environment with `nix`

```
nix-shell
```

After that you can use normal cabal workflow. Namely: `cabal new-build all` to build all packages, `cabal new-test all` to build and run tests

To install needed programs just use:

```
nix-env -i cabal-install
```

or

```
nix-env -i hlint
```


Then you can use `cabal` to build thundermit inside this shell or run `thundermint-simple` binary.

# Run testing network using terraform

First of all you have to install terraform and docker. Dowload `terraform` from [here](https://www.terraform.io/downloads.html), unzip and put in $PATH.

`terraform` runs nodes in separate docker containers. So, you need to build docker image.

## Build docker image

```
cd nix
build-docker.sh
```

All info about building docker images by `nix` I found [here](https://github.com/Gabriel439/haskell-nix/blob/master/project3/README.md#minimizing-the-closure).

When that command is finished you will have `result` simlink that refers to the docker image in tar archive format.

`build-docker.sh` loads this archive into local docker image.

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


# Migrations

Here we describe database migrations between different versions of thundermint

## 0.0.5 -> 0.0.6

Simple variant. drop table `wal` when node is stopped. Correct `wal` will be
created on node startup.

If write-ahead log is to be preserved following SQL script should be used

```
CREATE TABLE wal_bak AS SELECT DISTINCT FROM wal;
DROP TABLE wal;
CREATE TABLE wal
  ( id      INTEGER PRIMARY KEY
  , height  INTEGER NOT NULL
  , message BLOB NOT NULL
  , UNIQUE(height,message));
INSERT INTO wal SELECT * FROM wal_bak;
DROP TABLE wal_bak
```

