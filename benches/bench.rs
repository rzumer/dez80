#![allow(clippy::ptr_arg)]

#[macro_use]
extern crate criterion;

use criterion::{Bencher, Criterion, Fun};
use dez80::Instruction;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

const INSTRUCTION_STREAM: &[u8] = include_bytes!("../tests/allinstructions.bin");

fn bench_instruction_from_bytes(c: &mut Criterion) {
    c.bench_function("Instruction::from_bytes", |b| {
        b.iter(|| Instruction::decode_all(&mut INSTRUCTION_STREAM))
    });
}

fn bench_instruction_to_bytes(c: &mut Criterion) {
    fn bench_sequential(b: &mut Bencher, instructions: &Vec<Instruction>) {
        b.iter(|| instructions.iter().flat_map(|i| i.to_bytes()).collect::<Vec<u8>>())
    }

    fn bench_parallel(b: &mut Bencher, instructions: &Vec<Instruction>) {
        b.iter(|| instructions.par_iter().flat_map(|i| i.to_bytes()).collect::<Vec<u8>>())
    }

    let sequential_operations = Fun::new("Sequential", bench_sequential);
    let parallel_operations = Fun::new("Parallel", bench_parallel);

    let funs = vec![sequential_operations, parallel_operations];
    let mut instructions = Instruction::decode_all(&mut INSTRUCTION_STREAM);

    while instructions.len() < 1024 {
        instructions.append(&mut instructions.clone());
    }

    c.bench_functions("Instruction::to_bytes", funs, instructions);
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
    let mut instructions = Instruction::decode_all(&mut INSTRUCTION_STREAM);

    while instructions.len() < 1024 {
        instructions.append(&mut instructions.clone());
    }

    c.bench_functions("Instruction::to_string", funs, instructions);
}

criterion_group!(
    instruction,
    bench_instruction_from_bytes,
    bench_instruction_to_bytes,
    bench_instruction_to_string
);
criterion_main!(instruction);
