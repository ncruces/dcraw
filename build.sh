#!/bin/bash

set -Eeuo pipefail

JVER=2.1.5.1
if [ ! -d "libjpeg-turbo-$JVER" ]; then
  url="https://downloads.sourceforge.net/project/libjpeg-turbo/$JVER/libjpeg-turbo-$JVER-gcc64.exe"
  mkdir "libjpeg-turbo-$JVER"
  pushd "libjpeg-turbo-$JVER"
  curl -sL "$url" > archive.7z
  7z x archive.7z
  popd
fi

gcc -O3 -o dcraw.exe dcraw.c -lws2_32 -DNO_JASPER -DNO_LCMS \
    -I"libjpeg-turbo-$JVER/include" "libjpeg-turbo-$JVER/lib/libjpeg.dll.a"

cp "libjpeg-turbo-$JVER/bin/libjpeg-62.dll" .

7z a -mx9 dcraw-windows.zip dcraw.exe libjpeg-62.dll