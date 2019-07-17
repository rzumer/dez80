#[macro_use]
extern crate criterion;

use criterion::{Bencher, Criterion, Fun};
use dez80::{Instruction, MicroOperation};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

const INSTRUCTION_STREAM: &[u8] = &[
    0x00, // NOP
    0x01, 0x02, 0x03, // LD BC, **
    0x04, // INC B
    0x05, // DEC B
    0x06, 0x07, // LD B, *
    0x08, // RLCA
    0x09, // ADD HL, BC
    0x0A, // LD A, (BC)
];

fn bench_instruction_decode_all(c: &mut Criterion) {
    c.bench_function("Instruction::decode_all", |b| {
        b.iter(|| Instruction::from_bytes(&mut INSTRUCTION_STREAM))
    });
}

fn bench_instruction_operations(c: &mut Criterion) {
    fn bench_sequential(b: &mut Bencher, instructions: &Vec<Instruction>) {
        b.iter(|| instructions.iter().flat_map(|i| i.operations()).collect::<Vec<MicroOperation>>())
    }

    fn bench_parallel(b: &mut Bencher, instructions: &Vec<Instruction>) {
        b.iter(|| {
            instructions.par_iter().flat_map(|i| i.operations()).collect::<Vec<MicroOperation>>()
        })
    }

    let sequential_operations = Fun::new("Sequential", bench_sequential);
    let parallel_operations = Fun::new("Parallel", bench_parallel);

    let funs = vec![sequential_operations, parallel_operations];
    let mut instructions = Instruction::from_bytes(&mut INSTRUCTION_STREAM);

    while instructions.len() < 1024 {
        instructions.append(&mut instructions.clone());
    }

    c.bench_functions("Instruction::operations", funs, instructions);
}

fn bench_instruction_to_string(c: &mut Criterion) {
    fn bench_sequential(b: &mut Bencher, instructions: &Vec<Instruction>) {
        b.iter(|| instructions.iter().map(|i| i.to_string()).collect::<Vec<String>>())
    }

    fn bench_parallel(b: &mut Bencher, instructions: &Vec<Instruction>) {
        b.iter(|| instructions.par_iter().map(|i| i.to_string()).collect::<Vec<String>>())
    }

    let sequential_operations = Fun::new("Sequential", bench_sequential);
    let parallel_operations = Fun::new("Parallel", bench_parallel);

    let funs = vec![sequential_operations, parallel_operations];
    let mut instructions = Instruction::from_bytes(&mut INSTRUCTION_STREAM);

    while instructions.len() < 1024 {
        instructions.append(&mut instructions.clone());
    }

    c.bench_functions("Instruction::to_string", funs, instructions);
}

criterion_group!(
    instruction,
    bench_instruction_decode_all,
    bench_instruction_operations,
    bench_instruction_to_string
);
criterion_main!(instruction);
