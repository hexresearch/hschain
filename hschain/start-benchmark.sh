#!/bin/bash -e

trap 'kill 0' SIGINT

export GHCRTS="-N8"

#rm logs/*.log
#rm db/db*

# ~~~~~~~~~~~~~~~~~~~~

EXE=/home/dima/hschain/dist-newstyle/build/x86_64-linux/ghc-8.6.5/hschain-examples-0.1/x/hschain-coin-node/build/hschain-coin-node/hschain-coin-node

RTS="+RTS -p"

rm -rf n1
rm -rf n2
rm -rf n3
rm -rf n4

mkdir -p n1
pushd n1
$EXE --max-h 10 ../config/common.yaml ../config/local.yaml ../config/local1.yaml $RTS "$@" &
popd

mkdir -p n2
pushd n2
$EXE --max-h 10 ../config/common.yaml ../config/local.yaml ../config/local2.yaml $RTS "$@" &
popd

mkdir -p n3
pushd n3
$EXE --max-h 10 ../config/common.yaml ../config/local.yaml ../config/local3.yaml $RTS "$@" &
popd

mkdir -p n4
pushd n4
$EXE --max-h 10 ../config/common.yaml ../config/local.yaml ../config/local4.yaml $RTS "$@"
popd

kill 0
