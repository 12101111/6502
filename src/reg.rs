bitflags! {
/// NVsbdIZC
/// Note:
/// Two interrupts (/IRQ and /NMI) and two instructions (PHP and BRK)
/// push the flags to the stack.
/// In the byte pushed, bit 5 is always set to 1,
/// and bit 4 is 1 if from an instruction (PHP or BRK)
/// or 0 if from an interrupt line being pulled low (/IRQ or /NMI).
///
/// Instruction	Bits 5 and 4	Side effects after pushing
/// PHP	        11	            None
/// BRK	        11	            I is set to 1
/// /IRQ	    10	            I is set to 1
/// /NMI	    10	            I is set to 1
/// Two instructions (PLP and RTI) pull a byte from the stack
/// and set all the flags. They ignore bits 5 and 4.
    pub struct P:u8 {
        /// 7: N:Negative       1 if result's top bit is 1
        const N = 1 << 7;
        /// 6: V:Overflow       1 if result is overflow
        const V = 1 << 6;
        /// 5: s:               always 1
        const S = 1 << 5;
        /// 4: b:               see Note
        const B = 1 << 4;
        /// 3: d:Decimal        ignored by NES
        const D = 1 << 3;
        /// 2: I:interrupt      1 if disable interrupt
        const I = 1 << 2;
        /// 1: Z:Zero           1 if result is 0
        const Z = 1 << 1;
        /// 0: C:Carry          1 if result has carry(add) or borow(sub)
        const C = 1 << 0;
    }
}

#[allow(non_camel_case_types)]
pub struct Regs {
    // Auumulator
    pub a: u8,
    // index Register
    pub x: u8,
    // index Register
    pub y: u8,
    // Stack $100-$1FF
    // Stack Pointer
    pub s: u8,
    pub p: P,
    /// Program Counter
    pub pc: u16,
    pub cycle: usize,
}
impl Regs {
    pub fn new(pc: u16) -> Regs {
        Regs {
            a: 0,
            x: 0,
            y: 0,
            s: 0xfd,
            p: P::S | P::I,
            pc,
            cycle: 7,
        }
    }
    pub fn set_flags(&mut self, val: u8) {
        // Due to Note above , always set bit4 to 0 and bit5 to 1
        self.p = (P::from_bits(val).unwrap() | P::S) - P::B;
    }
    pub fn set_flag_zn(&mut self, val: u8) {
        self.p.set(P::Z, val == 0);
        self.p.set(P::N, (val & 0x80) != 0);
    }
    pub fn set_x(&mut self, val: u8) {
        self.x = val;
        self.set_flag_zn(val);
    }
    pub fn set_y(&mut self, val: u8) {
        self.y = val;
        self.set_flag_zn(val);
    }
    pub fn set_a(&mut self, val: u8) {
        self.a = val;
        self.set_flag_zn(val);
    }
}
