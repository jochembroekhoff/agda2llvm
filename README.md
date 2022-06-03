# agda2llvm: an Agda back-end for code extraction into LLVM IR

## About

agda2llvm is a the practical part of my contribution to the 2022 main edition of CSE3000 'Research Project', i.e. the Bachelor thesis.
The corresponding paper is yet to be published, but will be linked here once it is.

Practically, it is an experimental back-end for Agda which emits LLVM IR.
This, in turn, is compiled and linked by Clang into a native executable binary.

## Dependencies

You need the following things to successfully configure and build agda2llvm on your machine:

- Python 3 (tested with Python 3.10)
- Haskell Stack

When invoking agda2llvm, the following software needs to be available:

- `libgc` (Boehm Garbage Collector, tested with version 8.2.0)
- Clang (must be available on the path as `clang`, tested with Clang 13)

When executing a binary produced by agda2llvm, the following needs to be available on the machine:

- `libgc` (which must be ABI-compatible with the one present when producing the binary)

## Using agda2llvm

_These instructions assume you know how the normal Agda compiler works, globally._

Some preparatory work needs to be done before we can move on to actually using agda2llvm.
The `data` directory contains a Python script that we use to dynamically generate some data.
Run this first, from *within the data directory*, e.g. as follows:
```
$ pushd data
$ python gen_agda_prim.py
$ popd
```

Running from this repository can be done as demonstrated below.
Note that all arguments _after_ the double dash (`--`) are passed to agda2llvm, whereas everything _before_ that still applies to Stack.
```
$ stack build
$ stack run -- file.agda
```

To make agda2llvm available globally, run `stack install`.
The compiler is then available as `agda2llvm-exe`.

The build result will become available as `out.bin`, or `out.so` if compiling without main.
