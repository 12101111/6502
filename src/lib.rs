#![no_std]
#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate log;

mod address;
mod decode;
mod execute;
mod reg;
mod utils;

use reg::{Regs, P};

// IRQ/BRK<NMI<RESET
const NMI_VECTOR: u16 = 0xFFFA;
const RESET_VECTOR: u16 = 0xFFFC;
/// IRQ and BRK
const BRK_VECTOR: u16 = 0xFFFE;

pub trait Memory {
    fn reset(&mut self);
    fn loadb(&mut self, addr: u16) -> u8;
    fn try_loadb(&self, addr: u16) -> Option<u8>;
    fn storeb(&mut self, addr: u16, val: u8);
}

pub struct CPU<T: Memory> {
    pub cycle: usize,
    pub regs: Regs,
    pub mem: T,
}

impl<T: Memory> CPU<T> {
    pub fn new(mut mem: T) -> CPU<T> {
        CPU {
            // RESET execute in 6 cycles
            cycle: 7,
            regs: Regs::new(u16::from_le_bytes([
                mem.loadb(RESET_VECTOR),
                mem.loadb(RESET_VECTOR + 1),
            ])),
            mem,
        }
    }
    // External interrupts
    pub fn reset(&mut self) {
        self.regs = Regs::new(self.loadw(RESET_VECTOR));
        self.mem.reset();
        self.cycle = 7;
    }
    pub fn nmi(&mut self) {
        let pc = self.regs.pc;
        let mut flags = self.regs.p;
        flags -= P::B;
        self.pushw(pc);
        self.pushb(flags.bits());
        self.regs.p.insert(P::I);
        self.regs.pc = self.loadw(NMI_VECTOR);
    }
    pub fn irq(&mut self) {
        if !self.regs.p.contains(P::I) {
            let pc = self.regs.pc;
            let mut flags = self.regs.p;
            flags -= P::B;
            self.pushw(pc);
            self.pushb(flags.bits());
            self.regs.p.insert(P::I);
            self.regs.pc = self.loadw(BRK_VECTOR);
        }
    }
}
