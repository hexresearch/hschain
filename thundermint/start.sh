#!/usr/bin/env bash
set -e

trap 'kill 0' SIGINT

cabal new-run exe:thundermint-coin-node -- config/coin-node1.yaml "$@" &
cabal new-run exe:thundermint-coin-node -- config/coin-node2.yaml "$@" &
cabal new-run exe:thundermint-coin-node -- config/coin-node3.yaml "$@" &
cabal new-run exe:thundermint-coin-node -- config/coin-node4.yaml "$@" &

