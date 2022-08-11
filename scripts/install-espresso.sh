#!/bin/bash

set +x

if [ "$#" -ne 1 ]; then
    echo "Usage: scripts/install-espresso.sh INSTALL_PATH"
    exit 1
fi

echo "Installing espresso to $1"
echo "Make sure $1/bin is on your PATH before using Constellation"

git submodule update --init espresso
cd espresso
mkdir -p build
sed -i "s/3.19/3.10/g" CMakeLists.txt
cd build
cmake ../ -DBUILD_DOC=OFF -DCMAKE_INSTALL_PREFIX=$1
make install

echo "Espresso has been installed to $1/bin/espresso"
