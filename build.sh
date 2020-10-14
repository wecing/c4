#!/bin/sh

set -e

# Linux: `sudo apt install llvm`
# Mac: `brew install llvm` (keg-only is fine)
if [ `uname` != 'Linux' ]; then
  export LLVM_SYS_100_PREFIX="/usr/local/opt/llvm"
fi

if [ "$(diff proto/ast.proto pp/src/main/resources/ast.proto)" != "" ]; then
  cp proto/ast.proto pp/src/main/resources/ast.proto
fi

if [ "$(diff proto/ast.proto cc/ast.proto)" != "" ]; then
  cp proto/ast.proto cc/ast.proto
fi

cd pp
sbt assembly
cd ..

cd cc
cargo build
cd ..
