# c4

C's cool C89 compiler.

`c4 = pp |> cc |> opt |> as`

## Current status

- Preprocessor (`pp`): complete
- IR Emitter (`cc`): mostly complete
- Optimizer (`opt`): SSA construction / destruction only
- Assembler (`as`): X86-64 instruction selection & register allocation only

`cc` also supports emitting LLVM IR. With the LLVM backend, `c4` is able to compile Lua 5.3.0.

## Usage

Building and running `c4` requires LLVM 10, `sbt` (scala 3.0), `cargo` (rust 1.49), `dotnet` (F# 5.0), `stack` (Haskell GHC 8.10), and Python 3. It currently only runs on Linux.

    $ ./build.sh
    $ ./c4 --help
    $ ./c4 test.c
