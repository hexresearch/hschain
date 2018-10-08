#!/bin/bash -e

# Build with this command:
# stack build --library-profiling --executable-profiling --profile
#

export THUNDERMINT_KEYS="[\"2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y\",\"4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL\",\"3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS\",\"D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd\"]"

PREFIX=tmp/thundermint

COMMON_OPTIONS="--max-h 100 --delay 100 --check-consensus --deposit 1000 --keys 2000"

LOG_SPEC="\"nspecLogFile\" : [{ \"type\": \"ScribeES\", \"path\" : \"`cat elastic-search.cfg`\", \"severity\" : \"Debug\", \"verbosity\" : \"V2\" }]"
# LOG_SPEC="\"nspecLogFile\" : [{ \"type\": \"ScribeTXT\", \"severity\" : \"Debug\", \"verbosity\" : \"V2\" }]"

rm -rf $PREFIX
mkdir --parents $PREFIX
mkdir --parents $PREFIX/node-1/db
mkdir --parents $PREFIX/node-2/db
mkdir --parents $PREFIX/node-3/db
mkdir --parents $PREFIX/node-4/db

trap 'kill 0' SIGINT

bootstrap() {
    bs=$1
    ( echo -e $bs | nc 127.0.0.1 49999 -w 1 ) || ( sleep 0.1 ; bootstrap $bs > /dev/null 2> /dev/null )
}

echo "Node 1"
export THUNDERMINT_NODE_SPEC="{ \"nspecPrivKey\":\"2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y\", \"nspecDbName\": \"./db/node-1\", $LOG_SPEC, \"nspecWalletKeys\"  : [0,1]}"
( cd $PREFIX/node-1 ; stack exec thundermint-coin-node -- --listen-port 50001 $COMMON_OPTIONS & )
bootstrap "[\"127.0.0.1:50002\",\"127.0.0.1:50003\",\"127.0.0.1:50004\"]\n"
sleep 0.5

echo "Node 2"
export THUNDERMINT_NODE_SPEC="{ \"nspecPrivKey\":\"4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL\", \"nspecDbName\": \"./db/node-2\", $LOG_SPEC, \"nspecWalletKeys\"  : [1,1]}"
( cd $PREFIX/node-2 ; stack exec thundermint-coin-node -- --listen-port 50002 $COMMON_OPTIONS & )
bootstrap "[\"127.0.0.1:50003\",\"127.0.0.1:50004\"]\n"
sleep 0.5

echo "Node 3"
export THUNDERMINT_NODE_SPEC="{ \"nspecPrivKey\":\"3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS\", \"nspecDbName\": \"./db/node-3\", $LOG_SPEC, \"nspecWalletKeys\"  : [2,1]}"
( cd $PREFIX/node-3 ; stack exec thundermint-coin-node -- --listen-port 50003 $COMMON_OPTIONS & )
bootstrap "[\"127.0.0.1:50004\"]\n"
sleep 0.5

echo "Node 4"
export THUNDERMINT_NODE_SPEC="{ \"nspecPrivKey\":\"D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd\", \"nspecDbName\": \"./db/node-4\", $LOG_SPEC, \"nspecWalletKeys\"  : [3,1]}"
( cd $PREFIX/node-4 ; stack exec thundermint-coin-node -- --listen-port 50004 $COMMON_OPTIONS & )
bootstrap "[]\n"
sleep 0.5

echo "Ok, node started!"
echo "Waiting for node finished"
sleep 1h

echo "OK"
