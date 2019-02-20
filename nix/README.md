# xenochain-nix

The build scripts for robust and fast incremental building of xenochain.

You need:
- installed nix
- configured ssh access to other private repos on hexresearch github

# Building of backend

To enter build environment you need to just:
```
./shell.sh
```

Note: if you get errors about `repository doesn't exist`, try `ssh-add` key that has access to the repo.

After all dependencies are built, you will be in root folder pf the project in shell, where
`ghc`, `ghcid` and `cabal` are configured to work with all internal packages.

To build the backend:
```
cabal new-build all
```

To run result of build:
```
cabal new-run xenochain:xenochain-validator -- <pass args here>
```

To run tests:
```
cabal new-test all
```

If you want a functionality similar to `cabal --file-watch` to rebuild when files change:
```
ghcid -c "cabal new-repl xenochain-service"
```

To exit the shell, type `exit`. Besides, you might want to kill `socat` that is left at the background (only for NixOS):
```
sudo pkill -TERM socat
```

You can trigger non incremental build:
```
./build.sh
```

# Building frontend

See [README.md](../frontend/README.md)

# Adding dependencies

* You can add external dependencies (that are public third-party packages) using:
```
cabal2nix cabal://<package-name>-<version> > ./derivations/<package-name>.nix
```
Where you can use url for git repo instead of `cabal://`. Also you can add `--no-check` or `--no-haddock`.
The public dependencies are automatically added to environment from `derivations` folder when you enter the shell.

* External dependencies in private repos. Do the same as with a public dependency, but patch resulted nix file to
fetch from private repo:
```
src = tryEval <thundermint-src> (fetchgitPrivate pkgConfig.thundermint);
```  
And add coresponding item to `versions.json`:
```
"thundermint":  {
  "url":    "git@github.com:hexresearch/thundermint.git",
  "rev":    "613a42c24ee8dbe9e16d0c66f106fdd556b0c4a0",
  "sha256": "0kam8qqmj95p85ilrck5c052lj4c1rwr9r9y59chrml0z1ybpa0w"
},
```

You can temporarily use a local version of those dependencies by patching `shell.nix` as:
```
NIX_PATH=ssh-config-file=$SSH_CONFIG:ssh-auth-sock=/tmp/hax:crypto-history-src=./crypto-history:$NIX_PATH nix-shell --command "cd ../xenochain; return"
```

* Internal package. First, patch `release.nix` to add it to `haskellPackages`:
```
  ....
    exchange-stats = callInternal "exchange-stats" ../xenochain/exchange-stats { };
    hex-common = callInternal "hex-common" ../xenochain/hex-common { };
    hex-server-common = callInternal "hex-server-common" ../xenochain/hex-server-common { };
    <new-package> = callInternal "<new-package>" ../xenochain/<new-package> { };
  }
```
Second, add it to outputs in `release.nix`:
```
    crypto-history = pkgs.haskellPackages.crypto-history;
    nem-api = pkgs.haskellPackages.nem-api;
    haskoin-core = pkgs.haskellPackages.haskoin-core;
    docker-container = pkgs.docker-container;
    <new-package> = pkgs.<new-package>;
  };
  in self
```
Third, it is required to add package into production build:
```
cabal2nix --no-haddock git@github.com:hexresearch/xenochain.git --subpath SUB_PATH_IN_REPO > ./production/<package-name>.nix
```
And edit the `production/<package-name>.nix` the same way as the other files in the folder:
```
{ tryEval, mkDerivation, aeson, base, bytestring, containers, exchange-stats
, hex-common, hex-server-common, http-api-data, http-types
, interpolatedstring-perl6, mtl, natural-transformation, random
, scientific, servant, servant-server, stdenv, stm, text, time
, unix-time, vector, vector-space, wai, wai-cors, wai-extra, warp
}:
mkDerivation {
  pname = "tv-chart";
  version = "0.1.0.0";
  version = "0.1.0.0";
  src = tryEval <xenochain> (fetchgitPrivate pkgConfig.xenochain);
  postUnpack = "sourceRoot+=/tv-chart; echo source root reset to $sourceRoot";
  ....
```

Finally, add it to the incremental environment in `shell.nix`:
```
  xenochain-service
  xenochain-state
  xenochain-types
  <new-package>
  ];
```
Don't forget to add the new package to `xenochain/cabal.project` in the case if it is required.

# Updating dependencies

* Public dependencies, just call `cabal2nix` like in `Adding dependencies` part.
* Internal dependencies, update `versions.json` file.

# Building docker container

TODO

# Using netrc file

You can use netrc file to authentificate clone of private repos. To perform this, you need:

* Change url to `https` in `versions.json`

* Create file `.netrc` in the root of the repository, format:
```
machine github.com
username <github login>
login <github login>
password <password or github access token>
```

* Execute `./shell.sh`
