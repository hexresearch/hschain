#!/bin/bash -e

set -euo pipefail

#IFS='\n\t'

# Build with this command:
# stack build --library-profiling --executable-profiling --profile
#

export THUNDERMINT_KEYS="[\"2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y\",\"4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL\",\"3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS\",\"D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd\"]"

THUNDERMINT_COIN_NODE=/home/sz/work/thunders/thundermint/dist-newstyle/build/x86_64-linux/ghc-8.4.4/thundermint-0.1/x/thundermint-coin-node/build/thundermint-coin-node/thundermint-coin-node

PREFIX=tmp/thundermint

EXTRA=$*
echo "$EXTRA"

COMMON_OPTIONS='--total-nodes 4 --max-h 5 --delay 50 --check-consensus --deposit 1000 --keys 2000'

#LOG_SPEC="\"nspecLogFile\" : [{ \"type\": \"ScribeES\", \"path\" : \"`cat elastic-search.cfg`\", \"severity\" : \"Debug\", \"verbosity\" : \"V2\" }]"
LOG_SPEC='"nspecLogFile" : [{ "type": "ScribeJSON", "path" : "log.js", "severity" : "Debug", "verbosity" : "V2" }]'

rm -rf $PREFIX
mkdir --parents $PREFIX
mkdir --parents $PREFIX/node-1/db
mkdir --parents $PREFIX/node-2/db
mkdir --parents $PREFIX/node-3/db
mkdir --parents $PREFIX/node-4/db

trap 'kill -KILL 0' SIGINT EXIT

bootstrap() {
    bs=$1
    ( echo -e $bs | nc 127.0.0.1 49999 -w 1 > /dev/null 2> /dev/null ) || ( sleep 0.1 ; bootstrap $bs )
}

echo "Node 1"
export THUNDERMINT_NODE_SPEC="{ \"nspecPrivKey\":\"2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y\", \"nspecDbName\": \"./db/node-1\", $LOG_SPEC, \"nspecWalletKeys\"  : [0,1]}"
pushd $PREFIX/node-1 > /dev/null
    time -v $THUNDERMINT_COIN_NODE --node-n 0 --listen-port 50001 $COMMON_OPTIONS --peers "[\"127.0.0.1:50002\",\"127.0.0.1:50003\",\"127.0.0.1:50004\"]" $EXTRA >logs &
popd > /dev/null
sleep 0.5

echo "Node 2"
export THUNDERMINT_NODE_SPEC="{ \"nspecPrivKey\":\"4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL\", \"nspecDbName\": \"./db/node-2\", $LOG_SPEC, \"nspecWalletKeys\"  : [1,1]}"
pushd $PREFIX/node-2 > /dev/null
    time -v $THUNDERMINT_COIN_NODE --node-n 1 --listen-port 50002 $COMMON_OPTIONS --peers "[\"127.0.0.1:50003\",\"127.0.0.1:50004\"]" $EXTRA >logs  &
popd > /dev/null
sleep 0.5

echo "Node 3"
export THUNDERMINT_NODE_SPEC="{ \"nspecPrivKey\":\"3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS\", \"nspecDbName\": \"./db/node-3\", $LOG_SPEC, \"nspecWalletKeys\"  : [2,1]}"
pushd $PREFIX/node-3 > /dev/null
    time -v $THUNDERMINT_COIN_NODE --node-n 2 --listen-port 50003 $COMMON_OPTIONS --peers "[\"127.0.0.1:50004\"]" $EXTRA >logs &
popd > /dev/null
sleep 0.5

echo "Node 4"
export THUNDERMINT_NODE_SPEC="{ \"nspecPrivKey\":\"D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd\", \"nspecDbName\": \"./db/node-4\", $LOG_SPEC, \"nspecWalletKeys\"  : [3,1]}"
pushd $PREFIX/node-4 > /dev/null
    time -v $THUNDERMINT_COIN_NODE --node-n 3 --listen-port 50004 $COMMON_OPTIONS --peers "[]" $EXTRA >logs  &
popd > /dev/null
sleep 0.5

echo "Ok, node started!"
echo "Waiting for node finished"
wait
#sleep 600

echo "OK"
