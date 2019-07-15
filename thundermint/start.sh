#!/bin/bash

trap 'kill 0' SIGINT

export GHCRTS="-N8"
thundermint-coin-node config/common.yaml config/local.yaml config/local1.yaml "$@" &
thundermint-coin-node config/common.yaml config/local.yaml config/local2.yaml "$@" &
thundermint-coin-node config/common.yaml config/local.yaml config/local3.yaml "$@" &
thundermint-coin-node config/common.yaml config/local.yaml config/local4.yaml "$@"
kill 0
