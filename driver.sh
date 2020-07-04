#!/bin/sh

set -e

if [ `uname` = 'Linux' ]; then
  export LLVM_SYS_100_PREFIX=$(llvmenv prefix)
  export C4_TARGET_ARCH=k8
else
  export LLVM_SYS_100_PREFIX="$HOME/.local/share/llvmenv_fake/llvm-local"
  export C4_TARGET_ARCH=darwin
fi

bazel build ...

cd c4cc
cargo build

cd ..
echo
echo ====================
echo

./bazel-out/${C4_TARGET_ARCH}-fastbuild/bin/src/main/scala/c4/parser $1 \
  | ./c4cc/target/debug/c4cc
