#!/bin/bash -e

##
## Prepare environment for use "bls-signatures" package
##

# Download 'bls-signatures' library and all its dependencies
git submodule update --init --recursive

# Apply patch for prevent GMP linking to libbls.so
git apply bls-signatures/remove-gmp-from-linking.patch --directory=bls-signatures/bls-signatures || echo "Already patched"

# Build libbls.so
mkdir -p bls-signatures/bls-signatures/build
pushd bls-signatures/bls-signatures/build
cmake ../
cmake --build . --target combined_custom -- -j
popd

# Add path to libraries and headers
BLS=${PWD}/bls-signatures/bls-signatures
cabal new-configure \
    --extra-lib-dirs=${BLS}/build \
    --extra-include-dirs=${BLS}/src \
    --extra-include-dirs=${BLS}/build/contrib/relic/include \
    --extra-include-dirs=${BLS}/contrib/relic/include


# cabal new-build -j
# cabal new-test -j

