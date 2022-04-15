#!/bin/bash

set -euxo pipefail

# build with CMake
top=$(pwd)
cd cpp-bindings
mkdir build || (rm -r build && mkdir build)
cd build
cmake ..
make -j
cd "$top"

# copy artifacts over
mkdir -p lib/
cp \
  cpp-bindings/build/common/libcommon.so \
  cpp-bindings/build/jtinyC/jtinyC.jar \
  cpp-bindings/build/jtinyC/libjtinyC.so \
  cpp-bindings/build/jtiny86/jtiny86.jar \
  cpp-bindings/build/jtiny86/libjtiny86.so \
  lib/
