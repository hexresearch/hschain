#!/bin/bash

tclsh mock-consensus-node.tcl 22222                            \
        "cabal new-exec -- hschain-sql-utils wallet-demo-tables"
