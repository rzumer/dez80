# DeZ80 CLI

A Z80 disassembler interface based on the DeZ80 library.

The CLI accepts a file path as input, decodes Z80 opcodes,
and outputs standard mnemonics and operands to a text file or standard output.

## Usage

```
dez80_cli <INPUT_FILE> [OUTPUT_FILE]
```

## Example

Input file contents (hexadecimal):
```
00 01 02 03 04 05 06 07 08 09 0A
```

Output text:
```
NOP
LD BC, 0x0302
INC B
DEC B
LD B, 0x07
EX AF, AF'
ADD HL, BC
LD A, (BC)
```
