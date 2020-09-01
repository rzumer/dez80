# DeZ80

A Z80 instruction decoding and (dis)assembly library.

## Features

* Standard Z80 instruction decoding and encoding
* Implementation-independent data structures
* Stateful and stateless instruction decoding interfaces
* Format handlers for displaying instructions and their individual components

## Use Cases

DeZ80 can function as a simple disassembler.
The `dez80_cli` binary component provides an interface for this use case.
It can also reassemble defined `Instruction` structs,
but does not provide any text parsing functionality.

The library component is flexible enough for use in other applications,
such as assemblers, debuggers, or emulators.
