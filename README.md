# DeZ80

A Z80 disassembly library.

## Features

* Standard Z80 instruction decoding
* Implementation-independent data structures to facilitate handling of decoded instructions
* Decomposition of instructions into micro-operations with fixed clock cycle costs
* Format handlers for displaying instructions and their individual components

## Use Cases

DeZ80 can function as a simple disassembler.
The `dez80_cli` binary component provides an interface for this use case.

The library component is flexible enough for use in other applications,
such as assemblers, debuggers, or emulators.
