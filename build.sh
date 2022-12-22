#!/bin/bash

set -Eeuo pipefail

JVER=3.0.1
if [ ! -d "libjpeg-turbo-$JVER" ]; then
  url="https://downloads.sourceforge.net/project/libjpeg-turbo/$JVER/libjpeg-turbo-$JVER.tar.gz"
  curl -#L "$url" | tar xz
fi

BVER=version_116
if [ ! -d "binaryen-$BVER" ]; then
  url="https://github.com/WebAssembly/binaryen/releases/download/$BVER/binaryen-$BVER-x86_64-linux.tar.gz"
  curl -#L "$url" | tar xz
fi
PATH="$PWD/binaryen-$BVER/bin:$PATH"

WVER=20
if [ ! -d "wasi-sdk-$WVER.0" ]; then
  url="https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-$WVER/wasi-sdk-$WVER.0-linux.tar.gz"
  curl -#L "$url" | tar xz
fi
WASI_SDK_PREFIX="$PWD/wasi-sdk-$WVER.0"
PATH="$WASI_SDK_PREFIX/bin:$PATH"

mkdir -p build_wasm
pushd build_wasm
cmake "-DWASI_SDK_PREFIX=$WASI_SDK_PREFIX" "-DCMAKE_TOOLCHAIN_FILE=$WASI_SDK_PREFIX/share/cmake/wasi-sdk.cmake" \
  -G"Unix Makefiles" "../libjpeg-turbo-$JVER" \
  -D"CMAKE_C_FLAGS_RELEASE=-Oz -g0 -DNDEBUG" \
  -D"WITH_SIMD=0" \
  -D"WITH_TURBOJPEG=0"

make -j jpeg-static
clang --target=wasm32-wasi -g0 -flto \
       -Oz -o dcraw.wasm ../dcraw.c -DNO_JASPER -DNO_LCMS \
       -I. -I"../libjpeg-turbo-$JVER" \
       CMakeFiles/jpeg*-static.dir/*.obj
popd

wasm-opt build_wasm/dcraw.wasm -O4 -o dcraw.wasm
gzip -9f dcraw.wasm
