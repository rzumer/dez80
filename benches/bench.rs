#![allow(clippy::ptr_arg)]

#[macro_use]
extern crate criterion;

use criterion::{Bencher, Criterion};
use dez80::Instruction;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

const INSTRUCTION_STREAM: &[u8] = include_bytes!("../tests/allinstructions.bin");

fn bench_instruction_from_bytes(c: &mut Criterion) {
    c.bench_function("Instruction::from_bytes", |b| {
        b.iter(|| Instruction::decode_all(&mut &INSTRUCTION_STREAM[..]))
    });
}

fn bench_instruction_to_bytes(c: &mut Criterion) {
    fn bench_sequential(b: &mut Bencher, instructions: &Vec<Instruction>) {
        b.iter(|| instructions.iter().flat_map(|i| i.to_bytes()).collect::<Vec<u8>>())
    }

    fn bench_parallel(b: &mut Bencher, instructions: &Vec<Instruction>) {
        b.iter(|| instructions.par_iter().flat_map(|i| i.to_bytes()).collect::<Vec<u8>>())
    }

    let mut instructions = Instruction::decode_all(&mut &INSTRUCTION_STREAM[..]);
    while instructions.len() < 1024 {
        instructions.append(&mut instructions.clone());
    }

    let mut bench_group = c.benchmark_group("Instruction::to_bytes");
    bench_group.bench_with_input("Sequential", &instructions, bench_sequential);
    bench_group.bench_with_input("Parallel", &instructions, bench_parallel);
}

fn bench_instruction_to_string(c: &mut Criterion) {
    fn bench_sequential(b: &mut Bencher, instructions: &Vec<Instruction>) {
        b.iter(|| instructions.iter().map(|i| i.to_string()).collect::<Vec<String>>())
    }

    fn bench_parallel(b: &mut Bencher, instructions: &Vec<Instruction>) {
        b.iter(|| instructions.par_iter().map(|i| i.to_string()).collect::<Vec<String>>())
    }

    let mut instructions = Instruction::decode_all(&mut &INSTRUCTION_STREAM[..]);
    while instructions.len() < 1024 {
        instructions.append(&mut instructions.clone());
    }

    let mut bench_group = c.benchmark_group("Instruction::to_string");
    bench_group.bench_with_input("Sequential", &instructions, bench_sequential);
    bench_group.bench_with_input("Parallel", &instructions, bench_parallel);
}

criterion_group!(
    instruction,
    bench_instruction_from_bytes,
    bench_instruction_to_bytes,
    bench_instruction_to_string
);
criterion_main!(instruction);
