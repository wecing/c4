#!/bin/sh

set -e

# Linux: `sudo apt install llvm`
# Mac: `brew install llvm` (keg-only is fine)
if [ `uname` != 'Linux' ]; then
  export LLVM_SYS_100_PREFIX="/usr/local/opt/llvm"
fi

mkdir -p pp/src/main/java/c4/ast
java -jar pp/src/main/resources/java-cup-11b.jar \
  -parser C4Parser -symbols C4Symbols -destdir pp/src/main/java/c4/ast/ \
  pp/src/main/resources/parser.cup

cp proto/ast.proto pp/src/main/resources/ast.proto
cp proto/ast.proto cc/ast.proto
cp proto/ir.proto cc/ir.proto
cp proto/ir.proto opt/opt-proto/ir.proto

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
