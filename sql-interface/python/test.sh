#!/bin/bash

export HSCHAIN_NODE_INFO="127.0.0.1:22222"
export HSCHAIN_DRIVER_INFO="$HOME/usr/lib/libsqlite3odbc.so"
echo "running info"
python funds-wallet.py info
