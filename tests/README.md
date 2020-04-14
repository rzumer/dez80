## Integration tests

The binary stream used in the DeZ80 integration tests was pre-assembled using
[z80asm](https://www.nongnu.org/z80asm/). This ensures that the binary input
and the expected disassembly output correspond to one another.

`z80asm` does not support direct relative offset operands for conditional instructions such as `DJNZ` and `JR`.
It expects absolute addresses, labels or variables to be provided as operands.

Therefore, the following steps are necessary to reproduce the test stream:
1. Replace all instances of `0xfe` in `allinstructions.asm` with `$`
2. Run `z80asm` or another compatible assembler
3. If desired, revert the changes from step 1 to perform the integration test
