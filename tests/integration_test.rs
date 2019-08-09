use dez80::Instruction;
use std::io::{BufRead, BufReader};

#[test]
fn decode_official_instructions() {
    // Read the compiled instructions using a buffered reader.
    let input_bytes: &[u8] = include_bytes!("allinstructions.bin");
    let mut reader = BufReader::new(input_bytes);

    // Decode instructions in the byte stream.
    let instructions = Instruction::from_bytes(&mut reader);

    // Format decoded instructions to their mnemonic representations.
    let formatted = instructions.iter().map(|i| i.to_string()).collect::<Vec<String>>();

    // Read the expected output mnemonics using a buffered reader.
    let input_text: &[u8] = include_bytes!("allinstructions.asm");
    reader = BufReader::new(input_text);

    // Compare the expected output with the generated one for each instruction.
    for (expected, actual) in reader.lines().zip(formatted.iter()) {
        assert_eq!(expected.unwrap(), *actual);
    }
}
