extern crate clap;
#[cfg(feature = "rayon")]
extern crate rayon;

use clap::{App, Arg};
use dez80::Instruction;
#[cfg(feature = "rayon")]
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::fs::File;
use std::io::{BufReader, BufWriter, Write};

fn main() -> Result<(), std::io::Error> {
    macro_rules! cargo_env {
        ($name: expr) => {
            env!(concat!("CARGO_PKG_", $name))
        };
    }

    let matches = App::new(cargo_env!("NAME"))
        .version(cargo_env!("VERSION"))
        .author(cargo_env!("AUTHORS"))
        .about(cargo_env!("DESCRIPTION"))
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT_FILE")
                .help("The input file to disassemble")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .value_name("OUTPUT_FILE")
                .help("The output file in which to dump the assembly code")
                .index(2),
        )
        .get_matches();

    // Open the specified input file using a buffered reader.
    let input_file = File::open(matches.value_of("input").unwrap())?;
    let mut reader = BufReader::new(input_file);

    // Decode instructions in the byte stream.
    let instructions = Instruction::decode_all(&mut reader);

    // Format decoded instructions to their mnemonic representations.
    #[cfg(feature = "rayon")]
    let formatted =
        instructions.par_iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n");
    #[cfg(not(feature = "rayon"))]
    let formatted = instructions.iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n");

    // Write out the mnemonics to file or standard output.
    if let Some(output_path) = matches.value_of("output") {
        let mut writer = BufWriter::new(File::create(output_path)?);
        writer.write_all(formatted.as_bytes())?;
    } else {
        let mut writer = BufWriter::new(std::io::stdout());
        writer.write_all(formatted.as_bytes())?;
    };

    Ok(())
}
