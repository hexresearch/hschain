#!/usr/bin/env bash

source ./setup_git.sh

isGHCJS="false"
if [ "$1" == "GHCJS" ]; then
  echo "GHCJS build"
  isGHCJS="true"
fi

NIX_PATH=$GIT_NIX_PATH$NIX_PATH nix-build --arg isProd true \
                                          --arg isGHCJS $isGHCJS
