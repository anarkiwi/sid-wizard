#!/bin/bash

set -e
BR=$(pwd)

# 64tass >= 1.59 is required (1.97 was built with 1.59.3120 / 1.60.3243).
# Ubuntu 24.04 ships 1.59.3120; 22.04 is too old, so build on 24.04/latest.
# vice provides c1541 (used to build the .d64 images). The packer is now
# bundled (native/sources/tools/compress64kSFX), so exomizer is no longer needed.
sudo apt-get update && sudo apt-get -y install 64tass build-essential vice

cd "$BR/native/sources" && make clean && make
