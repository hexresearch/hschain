#!/bin/bash
# attempt to issue clean-configure-build-install commands.
# XXX TODO for now the install path is fixed to $HOME/usr ($HOME/usr/bin and $HOME/usr/lib).

# the strict mode: the execution of script will stop if any command
# has failed.
set -euo pipefail
IFS=$'\n\t'

make clean || echo "clean failed - perfectly valid situation"

./configure --prefix=$HOME/usr
make
make install
