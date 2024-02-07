extern crate clap;
#[cfg(feature = "rayon")]
extern crate rayon;

use clap::{crate_authors, crate_description, crate_name, crate_version, Arg, Command};
use dez80::Instruction;
#[cfg(feature = "rayon")]
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{
    fs::File,
    io::{BufReader, BufWriter, Write},
};

fn main() -> Result<(), std::io::Error> {
    let matches = Command::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!())
        .args([
            Arg::new("input")
                .short('i')
                .long("input")
                .value_name("INPUT_FILE")
                .help("The input file to disassemble")
                .required(true),
            Arg::new("output")
                .short('o')
                .long("output")
                .value_name("OUTPUT_FILE")
                .help("The output file in which to dump the assembly code"),
        ])
        .arg_required_else_help(true)
        .get_matches();

    // Open the specified input file using a buffered reader.
    let input_file = File::open(matches.get_one::<String>("input").unwrap())?;
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
    if let Some(output_path) = matches.get_one::<String>("output") {
        let mut writer = BufWriter::new(File::create(output_path)?);
        writer.write_all(formatted.as_bytes())?;
    } else {
        let mut writer = BufWriter::new(std::io::stdout());
        writer.write_all(formatted.as_bytes())?;
    };

    Ok(())
}
