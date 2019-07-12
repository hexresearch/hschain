#!/bin/bash -e
##
## Start some 'thundermint-coin-node' locally
##

# Additional arguments for running
ARGS="+RTS -N4"

# Clearing environment
rm db/*
rm logs/*

trap 'kill 0' SIGINT

echo "Starting local thundermint-coin-node cluster"
thundermint-coin-node config/common.yaml config/local.yaml config/local1.yaml $ARGS &
thundermint-coin-node config/common.yaml config/local.yaml config/local2.yaml $ARGS &
thundermint-coin-node config/common.yaml config/local.yaml config/local3.yaml $ARGS &
thundermint-coin-node config/common.yaml config/local.yaml config/local4.yaml $ARGS

kill 0
