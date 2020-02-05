source $stdenv/setup

set -x
set -e
# Unpack sourse
cp -r $src/* .
for p in $patches; do
    patch -p1 $p
done

# Start building
mkdir build
cd build
#
export HOME=$PWD
cmake .. \
  "-DCMAKE_TOOLCHAIN_FILE=${var_emscripten}/share/emscripten/cmake/Modules/Platform/Emscripten.cmake" \
  "-DCMAKE_CXX_FLAGS=-std=c++11"
cmake --build . --target blsjs

exit 12

