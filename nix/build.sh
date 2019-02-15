#!/usr/bin/env bash

source ./setup_git.sh

tag=$(git tag -l --points-at HEAD)
if [ ! -z $tag ]; then
  GIT_TAG_ARG="--arg gitTag \"\\\"$tag\\\"\""
fi

NIX_PATH=$GIT_NIX_PATH$NIX_PATH nix-build --arg isProd true $GIT_TAG_ARG
