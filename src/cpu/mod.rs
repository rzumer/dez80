#[macro_use]
pub mod micro_operation;
pub mod instruction;
pub mod storage;

use micro_operation::{MicroOperation, MicroOperationType};
use std::collections::VecDeque;
use storage::*;

/// Represents a Z80 CPU in the NEC µPD780 family.
#[derive(Default)]
pub struct CPU {
    registers: RegisterSet,
    work_queue: VecDeque<MicroOperation>,
    queued_cycles: usize,
}

impl CPU {
    fn reset(&mut self) {
        use RegisterPairType::*;

        let registers = &mut self.registers;
        registers.write_pair(AF, 0xFFFF);
        registers.write_pair(BC, 0xFFFF);
        registers.write_pair(DE, 0xFFFF);
        registers.write_pair(HL, 0xFFFF);

        registers.write_pair(IX, 0xFFFF);
        registers.write_pair(IY, 0xFFFF);

        registers.write_pair(PC, 0x0000);
        registers.write_pair(SP, 0xFFFF);

        registers.write_pair(IR, 0xFFFF);

        registers.write_pair(AF_, 0xFFFF);
        registers.write_pair(BC_, 0xFFFF);
        registers.write_pair(DE_, 0xFFFF);
        registers.write_pair(HL_, 0xFFFF);

        self.work_queue.clear();
        self.queued_cycles = 0;
    }

    fn run(&mut self, mut target_cycles: usize) {
        target_cycles += self.queued_cycles;
        self.queued_cycles = 0;
        let mut elapsed_cycles = 0;

        while let Some(operation) = self.work_queue.get(0) {
            // If the run condition of the queued micro-operation
            // is not fulfilled, skip it entirely.
            if let Some(condition) = operation.condition {
                if !condition.evaluate(&self.registers) {
                    self.work_queue.remove(0);
                    continue;
                }
            }

            if operation.cycles > target_cycles {
                break;
            } else {
                elapsed_cycles += operation.cycles;
            }

            match operation.r#type {
                MicroOperationType::NoOperation => (),
                _ => unimplemented!(),
            }

            self.work_queue.remove(0);
        }

        self.queued_cycles += target_cycles - elapsed_cycles;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use instruction::Condition;
    use micro_operation::NO_OP;

    #[test]
    fn queue_cycles() {
        let mut cpu: CPU = CPU::default();
        cpu.work_queue.push_back(NO_OP);

        cpu.run(2);
        assert_eq!(2, cpu.queued_cycles);

        cpu.run(4);
        assert_eq!(2, cpu.queued_cycles);

        cpu.work_queue.push_back(NO_OP);

        cpu.run(2);
        assert_eq!(0, cpu.queued_cycles);
    }

    #[test]
    fn skip_conditional_operations() {
        let mut cpu: CPU = CPU::default();
        cpu.registers.write(RegisterType::B, 0xFF);

        let conditional_operation = MicroOperation {
            r#type: MicroOperationType::NoOperation,
            cycles: 5,
            source: None,
            destination: None,
            condition: Some(Condition::RegisterZero(RegisterType::B)),
        };

        cpu.work_queue.push_back(conditional_operation);

        cpu.run(10);
        assert_eq!(10, cpu.queued_cycles);
        assert_eq!(0, cpu.work_queue.len());
    }
}
