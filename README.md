# Decaf Programming Language Compiler

## File Structure
- `main.cc` : Contains the mains function which calls the parser, drivers
and all visitors
- `ast.cc` : Implements the various AST Nodes
- `debug_visitor.cc` : Implements a simple visitor which prints the AST for
debugging purposes
- `codegen_visitor.cc` : Implements the visitor for the code generation and
semantic analysis pass
- `types.cc` : Defines some custom types and helper functions for operators and
return types
- `driver.cc` : Scaffolding code to run `flex` and `bison` with C++
- `decaf.ll` : Regular expressions for scanner genaration
- `decaf.yy` : Grammar rules for parsing
- `test_programs` : contains sample programs

## Run
1. `make clean`
 - optional
2. `make`
3. `./decaf inputFile.dcf > out`
 - redirect generated code from `stdout` to `out` file
 - errors and warnings are still printed to terminal through `stderr`
4. `lli out`
 - Runs the generated code

The above steps use the config utility `llvm-config`, which must be installed
on the system path and be available without specifying a version number.

This project uses the `clang++` compiler, since `llvm-config` produces some
flags which are not understood by `g++`.

## Description
The project combines the `flex` scanner generator and `bison` parser using scaffolding C++ code in `driver.cc`. During parsing, the Abstract Syntax Tree
is generated.
The use of C++ allows for design patters such as the visitor pattern to pass
over the AST for debugging and code generations purposes.
