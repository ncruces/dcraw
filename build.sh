#!/bin/bash

set -Eeuo pipefail

JVER=2.1.5.1
if [ ! -d "libjpeg-turbo-$JVER" ]; then
  url="https://downloads.sourceforge.net/project/libjpeg-turbo/$JVER/libjpeg-turbo-$JVER.tar.gz"
  curl -sL "$url" | tar vxz
fi

ZVER=0.10.0
if [ ! -d "zig-linux-x86_64-$ZVER" ]; then
  url="https://ziglang.org/download/$ZVER/zig-linux-x86_64-$ZVER.tar.xz"
  curl -sL "$url" | tar vxJ
fi
PATH="$PWD/zig-linux-x86_64-$ZVER:$PATH"

BVER=version_112
if [ ! -d "binaryen-$BVER" ]; then
  url="https://github.com/WebAssembly/binaryen/releases/download/$BVER/binaryen-$BVER-x86_64-linux.tar.gz"
  curl -sL "$url" | tar vxz
fi
PATH="$PWD/binaryen-$BVER/bin:$PATH"

mkdir -p build_wasm
pushd build_wasm
CC="zig cc --target=wasm32-wasi" cmake -G"Unix Makefiles" "../libjpeg-turbo-$JVER" \
  -D"CMAKE_C_FLAGS_RELEASE=-Oz -g0 -DNDEBUG" \
  -D"WITH_SIMD=0" \
  -D"WITH_TURBOJPEG=0"

make -j jpeg-static
zig cc --target=wasm32-wasi -g0 -flto \
       -Oz -o dcraw.wasm ../dcraw.c -DNO_JASPER -DNO_LCMS \
       -I. -I"../libjpeg-turbo-$JVER" \
       CMakeFiles/jpeg-static.dir/*.o \
       CMakeFiles/simd.dir/*.o
popd

wasm-opt build_wasm/dcraw.wasm -O4 -o dcraw.wasm
gzip -9f dcraw.wasm
