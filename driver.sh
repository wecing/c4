#!/bin/sh

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

if [ "$1" != '' ]; then
  cd ..
  echo
  echo ====================
  echo

  java -jar ./pp/target/scala-2.12/c4-assembly-1.0.jar $1 \
    | ./cc/target/debug/c4cc
fi
