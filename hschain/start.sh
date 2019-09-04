#!/bin/bash

trap 'kill 0' SIGINT

export GHCRTS="-N8"
hschain-coin-node config/common.yaml config/local.yaml config/local1.yaml "$@" &
hschain-coin-node config/common.yaml config/local.yaml config/local2.yaml "$@" &
hschain-coin-node config/common.yaml config/local.yaml config/local3.yaml "$@" &
hschain-coin-node config/common.yaml config/local.yaml config/local4.yaml "$@"
kill 0
