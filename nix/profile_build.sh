#!/usr/bin/env bash

source ./setup_git.sh

NIX_PATH=$GIT_NIX_PATH$NIX_PATH nix-build --arg isProd true --arg isProfile true
