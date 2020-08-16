#!/bin/sh

set -e

if [ `uname` = 'Linux' ]; then
  # `sudo apt install llvm`
  export C4_TARGET_ARCH=k8
else
  # `brew install llvm` (keg-only is fine)
  export LLVM_SYS_100_PREFIX="/usr/local/opt/llvm"
  export C4_TARGET_ARCH=darwin
fi

bazel build ...

cd c4cc
cargo build

if [ "$1" != '' ]; then
  cd ..
  echo
  echo ====================
  echo

  ./bazel-out/${C4_TARGET_ARCH}-fastbuild/bin/src/main/scala/c4/parser $1 \
    | ./c4cc/target/debug/c4cc
fi
