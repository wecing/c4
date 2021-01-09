#!/bin/bash

set -e

# Linux: `sudo apt install llvm`
# Mac: `brew install llvm` (keg-only is fine)
if [ `uname` != 'Linux' ]; then
  export LLVM_SYS_100_PREFIX="/usr/local/opt/llvm"
fi

# is_updated <SRC> <GEN>
function is_updated {
  test -f $2 && test $2 -nt $1
}

function cp_if_outdated {
  is_updated $1 $2 || cp $1 $2
}

mkdir -p pp/src/main/java/c4/ast
(is_updated pp/src/main/resources/parser.cup \
            pp/src/main/java/c4/ast/C4Parser.java &&
 is_updated pp/src/main/resources/parser.cup \
            pp/src/main/java/c4/ast/C4Symbols.java) ||
java -jar pp/src/main/resources/java-cup-11b.jar \
  -parser C4Parser -symbols C4Symbols -destdir pp/src/main/java/c4/ast/ \
  pp/src/main/resources/parser.cup

cp_if_outdated proto/ast.proto pp/src/main/resources/ast.proto
cp_if_outdated proto/ast.proto cc/ast.proto
cp_if_outdated proto/ir.proto cc/ir.proto
cp_if_outdated proto/ir.proto opt/opt-proto/ir.proto

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
