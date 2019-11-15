#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

export HSCHAIN_NODE_INFO="127.0.0.1 22222"
export HSCHAIN_DRIVER_INFO="$HOME/usr/lib/libsqlite3odbc-0.9996-hschain.so"
export HSCHAIN_PUBLIC_KEY=u1
echo "running info"
python funds-wallet.py info
