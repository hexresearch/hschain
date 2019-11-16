#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

export HSCHAIN_NODE_INFO="127.0.0.1 22222"
export HSCHAIN_DRIVER_INFO="$HOME/usr/lib/libsqlite3odbc-0.9996-hschain.so"
export HSCHAIN_PUBLIC_KEY=u1
echo "running info"
python funds-wallet.py info
echo "doing the transfer"
python funds-wallet.py transfer --to=u2 --amount=1
echo "some sleep for transaction to clear"
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
sleep 1
python funds-wallet.py info
