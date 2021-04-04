#!/bin/bash

set -e

# Linux: `sudo apt install llvm`
# Mac: `brew install llvm` (keg-only is fine)
if [ `uname` != 'Linux' ]; then
  export LLVM_SYS_100_PREFIX="/usr/local/opt/llvm"
fi

cd pp
sbt assembly
cd ..

cd cc
cargo build
cd ..

cd opt/opt-proto
dotnet build
cd ..
dotnet build
cd ..

cd as
stack build --copy-bins
cd ..
