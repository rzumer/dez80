mod operation;
mod storage;

use operation::Operation;
use std::collections::VecDeque;
use storage::*;

/// Represents a Z80 CPU in the NEC µPD780 family.
#[derive(Default)]
pub struct CPU {
    registers: RegisterSet,
    work_queue: VecDeque<Operation>,
    queued_cycles: usize
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
            if operation.cycles > target_cycles {
                break;
            } else {
                elapsed_cycles += operation.cycles;
            }

            match operation {
                _ => {
                    // NOP
                }
            }

            self.work_queue.remove(0);
        }

        self.queued_cycles += target_cycles - elapsed_cycles;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use operation::*;

    #[test]
    fn run_cpu() {
        let mut cpu: CPU = CPU::default();
        assert_eq!(0, cpu.queued_cycles);

        cpu.work_queue.push_back(NO_OP);

        cpu.run(2);
        assert_eq!(2, cpu.queued_cycles);

        cpu.run(4);
        assert_eq!(2, cpu.queued_cycles);

        cpu.work_queue.push_back(NO_OP);

        cpu.run(2);
        assert_eq!(0, cpu.queued_cycles);
    }
}
