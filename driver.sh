#!/bin/sh

set -e

export LLVM_SYS_90_PREFIX="$HOME/.local/share/llvmenv_fake/llvm-local"

bazel build ...

cd c4cc
cargo build

cd ..
echo
echo ====================
echo

./bazel-out/darwin-fastbuild/bin/src/main/scala/c4/parser $1 \
  | ./c4cc/target/debug/c4cc
