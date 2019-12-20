#!/bin/sh
trap 'kill 0' SIGINT
export GHCRTS="-N8 -A64M -s"
#
taskset -c 0-15 hschain-dioxane-node config/common.yaml config/local1.yaml "$@" &> /dev/null &
taskset -c 0-15 hschain-dioxane-node config/common.yaml config/local2.yaml "$@" &> /dev/null &
taskset -c 0-15 hschain-dioxane-node config/common.yaml config/local3.yaml "$@" &> /dev/null &
taskset -c 0-15 hschain-dioxane-node config/common.yaml config/local4.yaml "$@"
kill 0

