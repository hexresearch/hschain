#!/bin/bash -e
export THUNDERMINT_KEYS="[\"2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y\",\"4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL\",\"3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS\",\"D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd\"]"

PREFIX=tmp/thundermint

COMMON_OPTIONS="--max-h 100 --delay 100 --check-consensus --deposit 1000 --keys 2000 +RTS -hr -L75 -Pa"

rm -rf $PREFIX
mkdir -p $PREFIX
mkdir $PREFIX/node-1
mkdir $PREFIX/node-2
mkdir $PREFIX/node-3
mkdir $PREFIX/node-4

trap 'kill 0' SIGINT

echo "Node 1"
export THUNDERMINT_NODE_SPEC="{ \"nspecPrivKey\":\"2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y\", \"nspecDbName\": \"db/node-1\", \"nspecLogFile\" : [{ \"type\": \"ScribeTXT\", \"path\" : \"logs/node-1\", \"severity\" : \"Debug\", \"verbosity\" : \"V2\" }], \"nspecWalletKeys\"  : [0,1]}"
pushd $PREFIX/node-1 > /dev/null
stack exec thundermint-coin-node -- --listen-port 50001 $COMMON_OPTIONS &
pid1=$!
popd > /dev/null
sleep 0.5
echo -e "[\"127.0.0.1:50002\",\"127.0.0.1:50003\",\"127.0.0.1:50004\"]\n"  | nc 127.0.0.1 49999 -w 1
sleep 0.5

echo "Node 2"
export THUNDERMINT_NODE_SPEC="{ \"nspecPrivKey\":\"4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL\", \"nspecDbName\": \"db/node-2\", \"nspecLogFile\" : [{ \"type\": \"ScribeTXT\", \"path\" : \"logs/node-2\", \"severity\" : \"Debug\", \"verbosity\" : \"V2\" }], \"nspecWalletKeys\"  : [1,1]}"
pushd $PREFIX/node-2 > /dev/null
stack exec thundermint-coin-node -- --listen-port 50002 $COMMON_OPTIONS &
pid2=$!
popd > /dev/null
sleep 0.5
echo -e "[\"127.0.0.1:50003\",\"127.0.0.1:50004\"]\n"  | nc 127.0.0.1 49999 -w 1
sleep 0.5

echo "Node 3"
export THUNDERMINT_NODE_SPEC="{ \"nspecPrivKey\":\"3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS\", \"nspecDbName\": \"db/node-3\", \"nspecLogFile\" : [{ \"type\": \"ScribeTXT\", \"path\" : \"logs/node-3\", \"severity\" : \"Debug\", \"verbosity\" : \"V2\" }], \"nspecWalletKeys\"  : [2,1]}"
pushd $PREFIX/node-3 > /dev/null
stack exec thundermint-coin-node -- --listen-port 50003 $COMMON_OPTIONS &
pid3=$!
popd > /dev/null
sleep 0.5
echo -e "[\"127.0.0.1:50004\"]\n"  | nc 127.0.0.1 49999 -w 1
sleep 0.5

echo "Node 4"
export THUNDERMINT_NODE_SPEC="{ \"nspecPrivKey\":\"D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd\", \"nspecDbName\": \"db/node-4\", \"nspecLogFile\" : [{ \"type\": \"ScribeTXT\", \"path\" : \"logs/node-4\", \"severity\" : \"Debug\", \"verbosity\" : \"V2\" }], \"nspecWalletKeys\"  : [3,1]}"
pushd $PREFIX/node-4 > /dev/null
stack exec thundermint-coin-node -- --listen-port 50004 $COMMON_OPTIONS &
pid4=$!
popd > /dev/null
sleep 0.5
echo -e "[]\n"  | nc 127.0.0.1 49999 -w 1
sleep 0.5

echo "Ok, node started!"
echo "Waiting for node finished"
wait

echo "OK"
