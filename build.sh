#!/bin/bash

set -Eeuo pipefail

JVER=2.1.4
if [ ! -d "libjpeg-turbo-$JVER" ]; then
  url="https://downloads.sourceforge.net/project/libjpeg-turbo/$JVER/libjpeg-turbo-$JVER.tar.gz"
  curl -sL "$url" | tar xz
fi

if [[ "$OSTYPE" == "darwin"* ]]; then

mkdir -p build_arm
pushd build_arm
cmake -G"Unix Makefiles" "../libjpeg-turbo-$JVER" \
  -D"CMAKE_OSX_DEPLOYMENT_TARGET=11" \
  -D"CMAKE_OSX_ARCHITECTURES=arm64"
make -j jpeg-static
cc -O3 -o dcraw ../dcraw.c -DNO_JASPER -DNO_LCMS \
   -I. -I"../libjpeg-turbo-$JVER" libjpeg.a \
   --target=arm64-apple-macos11
popd

mkdir -p build_x86
pushd build_x86
cmake -G"Unix Makefiles" "../libjpeg-turbo-$JVER" \
  -D"CMAKE_OSX_DEPLOYMENT_TARGET=10.13" \
  -D"CMAKE_OSX_ARCHITECTURES=x86_64"
make -j jpeg-static
cc -O3 -o dcraw ../dcraw.c -DNO_JASPER -DNO_LCMS \
   -I. -I"../libjpeg-turbo-$JVER" libjpeg.a \
   --target=x86_64-apple-macos10.13
popd

lipo -create -output dcraw build_arm/dcraw build_x86/dcraw

else

mkdir -p build

pushd build
cmake -G"Unix Makefiles" "../libjpeg-turbo-$JVER"
make -j jpeg-static
cc -O3 -o ../dcraw ../dcraw.c -lm -DNO_JASPER -DNO_LCMS \
   -I. -I"../libjpeg-turbo-$JVER" libjpeg.a
popd

fi

gzip -9f dcraw