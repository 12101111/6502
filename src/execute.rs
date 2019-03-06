use crate::address::ins_size;
use crate::decode::{DECODE, OP::*};
use crate::reg::P;
use crate::BRK_VECTOR;
use crate::{Memory, CPU};
//passby borrowck
macro_rules! ap {
    ($f:expr,$i:expr) => {{
        let val = $i;
        $f(val)
    }};
    ($f:expr,$i:expr,$j:expr) => {{
        let val = $i;
        $f(val, $j)
    }};
}
impl<T: Memory> CPU<T> {
    pub fn execute(&mut self) {
        #[cfg(feature = "disasm")]
        {
            info!("{}", self);
        }
        let code = self.loadb_pc();
        let (op, am, ticks) = DECODE[code as usize].clone();
        self.regs.pc += 1;
        match op {
            LDA => ap!(|val| self.regs.set_a(val), self.load(&am)),
            LDX => ap!(|val| self.regs.set_x(val), self.load(&am)),
            LDY => ap!(|val| self.regs.set_y(val), self.load(&am)),
            STA => self.store(&am, self.regs.a),
            STX => self.store(&am, self.regs.x),
            STY => self.store(&am, self.regs.y),
            TAX => self.regs.set_x(self.regs.a),
            TAY => self.regs.set_y(self.regs.a),
            TXA => self.regs.set_a(self.regs.x),
            TYA => self.regs.set_a(self.regs.y),
            TSX => self.regs.set_x(self.regs.s),
            TXS => self.regs.s = self.regs.x,
            PHA => self.pushb(self.regs.a),
            PHP => self.pushb((self.regs.p | P::S | P::B).bits()),
            PLA => ap!(|val| self.regs.set_a(val), self.popb()),
            PLP => ap!(|val| self.regs.set_flags(val), self.popb()),
            ADC => {
                let val = self.load(&am);
                let a = self.regs.a;
                let mut result = a as u16 + val as u16;
                if self.regs.p.contains(P::C) {
                    result += 1;
                }
                self.regs.p.set(P::C, (result & 0x100) != 0);
                let result = result as u8;
                let flag = (a ^ val) & 0x80 == 0x0 && (a ^ result) & 0x80 == 0x80;
                self.regs.p.set(P::V, flag);
                self.regs.set_a(result);
            }
            SBC => {
                let val = self.load(&am);
                let a = self.regs.a;
                let mut result = a as i16 - val as i16;
                if !self.regs.p.contains(P::C) {
                    result -= 1;
                }
                self.regs.p.set(P::C, (result & 0x100) == 0);
                let result = result as u8;
                let flag = (a ^ result) & 0x80 != 0x0 && (a ^ val) & 0x80 == 0x80;
                self.regs.p.set(P::V, flag);
                self.regs.set_a(result);
            }
            INX => self.regs.set_x(self.regs.x.wrapping_add(1)),
            INY => self.regs.set_y(self.regs.y.wrapping_add(1)),
            DEX => self.regs.set_x(self.regs.x.wrapping_sub(1)),
            DEY => self.regs.set_y(self.regs.y.wrapping_sub(1)),
            INC => {
                let val = self.load(&am).wrapping_add(1);
                self.regs.set_flag_zn(val);
                self.store(&am, val);
            }
            DEC => {
                let val = self.load(&am).wrapping_sub(1);
                self.regs.set_flag_zn(val);
                self.store(&am, val);
            }
            AND => ap!(|i| self.regs.set_a(i), self.regs.a & self.load(&am)),
            ORA => ap!(|i| self.regs.set_a(i), self.regs.a | self.load(&am)),
            EOR => ap!(|i| self.regs.set_a(i), self.regs.a ^ self.load(&am)),
            BIT => {
                let val = self.load(&am);
                let a = self.regs.a;
                self.regs.p.set(P::Z, (val & a) == 0x0);
                self.regs.p.set(P::N, (val & 0x80) != 0);
                self.regs.p.set(P::V, (val & 0x40) != 0);
            }
            CMP => {
                let val = self.load(&am);
                let a = self.regs.a;
                let val = (a as u16).wrapping_sub(val as u16);
                self.regs.p.set(P::C, val < 0x100);
                self.regs.set_flag_zn(val as u8);
            }
            CPX => {
                let val = self.load(&am);
                let x = self.regs.x;
                let val = (x as u16).wrapping_sub(val as u16);
                self.regs.p.set(P::C, val < 0x100);
                self.regs.set_flag_zn(val as u8);
            }
            CPY => {
                let val = self.load(&am);
                let y = self.regs.y;
                let val = (y as u16).wrapping_sub(val as u16);
                self.regs.p.set(P::C, val < 0x100);
                self.regs.set_flag_zn(val as u8);
            }
            ASL => {
                let val = self.load(&am);
                self.regs.p.set(P::C, (val & 0x80) != 0);
                let val = val << 1;
                self.regs.set_flag_zn(val);
                self.store(&am, val);
            }
            LSR => {
                let val = self.load(&am);
                self.regs.p.set(P::C, (val & 0x1) != 0);
                let val = val >> 1;
                self.regs.set_flag_zn(val);
                self.store(&am, val);
            }
            ROL => {
                let mut val = self.load(&am);
                let carry = self.regs.p.contains(P::C);
                self.regs.p.set(P::C, (val & 0x80) != 0);
                val = val << 1;
                if carry {
                    val |= 0x1;
                }
                self.regs.set_flag_zn(val);
                self.store(&am, val);
            }
            ROR => {
                let mut val = self.load(&am);
                let carry = self.regs.p.contains(P::C);
                self.regs.p.set(P::C, (val & 0x1) != 0);
                val = val >> 1;
                if carry {
                    val |= 0x80;
                }
                self.regs.set_flag_zn(val);
                self.store(&am, val);
            }
            CLC => self.regs.p.set(P::C, false),
            CLD => self.regs.p.set(P::D, false),
            CLI => self.regs.p.set(P::I, false),
            CLV => self.regs.p.set(P::V, false),
            SEC => self.regs.p.set(P::C, true),
            SED => self.regs.p.set(P::D, true),
            SEI => self.regs.p.set(P::I, true),
            NOP => {
                let _ = self.load(&am);
            }
            ALR => {
                let val = self.load(&am);
                let mut a = self.regs.a;
                a &= val;
                self.regs.p.set(P::C, a & 0x1 != 0);
                a = a >> 1;
                self.regs.set_a(a);
            }
            ANC => {
                let val = self.load(&am);
                let a = self.regs.a;
                self.regs.set_a(a & val);
                self.regs.p.set(P::C, self.regs.p.contains(P::N))
            }
            ARR => {
                let val = self.load(&am);
                let mut a = self.regs.a;
                a &= val;
                a = a >> 1;
                if self.regs.p.contains(P::C) {
                    a |= 0x80;
                }
                self.regs.set_a(a);
                self.regs.p.set(P::C, a & 0x20 != 0);
                self.regs.p.set(P::V, (a >> 5) ^ (a >> 6) & 0x1 != 0);
            }
            AXS => {
                let val = (self.regs.a & self.regs.x) as u16 - self.load(&am) as u16;
                self.regs.set_x(val as u8);
                self.regs.p.set(P::C, val >> 15 != 0);
            }
            LAX => {
                let val = self.load(&am);
                self.regs.set_a(val);
                self.regs.set_x(val);
            }
            SAX => self.store(&am, self.regs.a & self.regs.x),
            DCP => {
                let val = self.load(&am).wrapping_sub(1);
                self.store(&am, val);
                let val = (self.regs.a as u16).wrapping_sub(val as u16);
                self.regs.p.set(P::C, val < 0x100);
                self.regs.set_flag_zn(val as u8);
            }
            ISC => {
                let val = self.load(&am).wrapping_add(1);
                self.regs.set_flag_zn(val);
                self.store(&am, val);
                let a = self.regs.a;
                let mut result = (a as u16).wrapping_sub(val as u16);
                if !self.regs.p.contains(P::C) {
                    result -= 1;
                }
                self.regs.p.set(P::C, (result & 0x100) == 0);
                let result = result as u8;
                let flag = (a ^ result) & 0x80 != 0x0 && (a ^ val) & 0x80 == 0x80;
                self.regs.p.set(P::V, flag);
                self.regs.set_a(result);
            }
            RLA => {
                let mut val = self.load(&am);
                let carry = self.regs.p.contains(P::C);
                let new_carry = (val & 0x80) != 0;
                self.regs.p.set(P::C, new_carry);
                val = val << 1;
                if carry {
                    val |= 0x1;
                }
                self.regs.set_flag_zn(val);
                self.store(&am, val);
                self.regs.a &= val;
                self.regs.set_flag_zn(self.regs.a)
            }
            RRA => {
                let mut val = self.load(&am);
                let carry = self.regs.p.contains(P::C);
                let new_carry = (val & 0x1) != 0;
                self.regs.p.set(P::C, new_carry);
                val = val >> 1;
                if carry {
                    val |= 0x80;
                }
                self.regs.set_flag_zn(val);
                self.store(&am, val);
                let a = self.regs.a;
                let mut result = a as u16 + val as u16;
                if new_carry {
                    result += 1;
                }
                self.regs.p.set(P::C, (result & 0x100) != 0);
                let result = result as u8;
                self.regs
                    .p
                    .set(P::V, (a ^ val) & 0x80 == 0x0 && (a ^ result) & 0x80 == 0x80);
                self.regs.set_a(result);
            }
            SLO => {
                let val = self.load(&am);
                self.regs.p.set(P::C, (val & 0x80) != 0);
                let val = val << 1;
                self.regs.set_flag_zn(val);
                self.store(&am, val);
                let a = self.regs.a;
                self.regs.set_a(a | val)
            }
            SRE => {
                let val = self.load(&am);
                self.regs.p.set(P::C, (val & 0x1) != 0);
                let val = val >> 1;
                self.regs.set_flag_zn(val);
                self.store(&am, val);
                let a = self.regs.a;
                self.regs.set_a(a ^ val)
            }
            SHY => unimplemented!(),
            SHX => unimplemented!(),
            STP => error!("STP"),
            XXA => unimplemented!(),
            AHX => unimplemented!(),
            TAS => unimplemented!(),
            LAS => unimplemented!(),
            _ => { /*JUMP*/ }
        };
        self.regs.pc = match op {
            JMP => self.get_addr(&am),
            JSR => {
                self.pushw(self.regs.pc - 1 + ins_size(&am));
                self.get_addr(&am)
            }
            RTS => self.popw() + 1,
            RTI => {
                let flags = self.popb();
                self.regs.set_flags(flags);
                self.popw()
            }
            BRK => {
                self.pushw(self.regs.pc + 1);
                self.pushb((self.regs.p | P::S | P::B).bits()); //See Note in reg.rs
                self.regs.p.set(P::I, true);
                self.loadw(BRK_VECTOR)
            }
            BEQ => self.branch(self.regs.p.contains(P::Z)),
            BNE => self.branch(!self.regs.p.contains(P::Z)),
            BCS => self.branch(self.regs.p.contains(P::C)),
            BCC => self.branch(!self.regs.p.contains(P::C)),
            BVS => self.branch(self.regs.p.contains(P::V)),
            BVC => self.branch(!self.regs.p.contains(P::V)),
            BMI => self.branch(self.regs.p.contains(P::N)),
            BPL => self.branch(!self.regs.p.contains(P::N)),
            _ => self.regs.pc + ins_size(&am),
        };
        self.cycle += ticks as usize;
    }
}

#[cfg(feature = "disasm")]
impl<'a, T: Controller> core::fmt::Display for CPU<'a, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        let code = self.try_loadb(self.regs.pc).unwrap();
        let (op, am, _) = DECODE[code as usize].clone();
        write!(f, "{:04X} {:02X} ", self.regs.pc, code)?;
        match am_bytes_size(&am) {
            0 => write!(f, "      ")?,
            1 => write!(f, "{:02X}    ", self.try_loadb(self.regs.pc + 1).unwrap())?,
            2 => write!(
                f,
                "{:02X} {:02X} ",
                self.try_loadb(self.regs.pc + 1).unwrap(),
                self.try_loadb(self.regs.pc + 2).unwrap()
            )?,
            _ => unreachable!(),
        }
        use super::address::AM::*;
        write!(f, "{:?} ", op)?;
        match am {
            IMP => write!(f, "                    ")?,
            IMM => write!(
                f,
                "#${:02X}                ",
                self.try_loadb(self.regs.pc + 1).unwrap()
            )?,
            ACC => write!(f, "A                   ")?,
            ABS => {
                let addr = self.try_loadw(self.regs.pc + 1).unwrap();
                match self.try_loadb(addr) {
                    Some(data) => write!(f, "${:04X} = {:02X}          ", addr, data)?,
                    None => write!(f, "${:04X} REG           ", addr)?,
                }
            }
            ZPG => {
                let addr = self.try_loadb(self.regs.pc + 1).unwrap();
                match self.try_loadb(addr as u16) {
                    Some(data) => write!(f, "${:02X} = {:02X}            ", addr, data)?,
                    None => write!(f, "${:02X} REG             ", addr)?,
                }
            }
            ABX | abx => {
                let addr = self.try_loadw(self.regs.pc + 1).unwrap();
                let addr_x = addr.wrapping_add(self.regs.x as u16);
                match self.try_loadb(addr_x) {
                    Some(data) => write!(f, "${:04X},X @ {:04X} = {:02X} ", addr, addr_x, data)?,
                    None => write!(f, "${:04X},X @ {:04X} REG  ", addr, addr_x)?,
                }
            }
            ABY | aby => {
                let addr = self.try_loadw(self.regs.pc + 1).unwrap();
                let addr_y = addr.wrapping_add(self.regs.y as u16);
                match self.try_loadb(addr_y) {
                    Some(data) => write!(f, "${:04X},Y @ {:04X} = {:02X} ", addr, addr_y, data)?,
                    None => write!(f, "${:04X},Y @ {:04X} REG  ", addr, addr_y,)?,
                }
            }
            REL => {
                let addr = self.try_loadb(self.regs.pc + 1).unwrap();
                let new_pc = ((addr as i8) as i32 + self.regs.pc as i32 + 2) as u16;
                write!(f, "${:04X}               ", new_pc)?
            }
            ZPX => {
                let addr = self.try_loadb(self.regs.pc + 1).unwrap();
                let addr_x = addr.wrapping_add(self.regs.x) as u16;
                match self.try_loadb(addr_x) {
                    Some(data) => {
                        write!(f, "${:02X},X @ {:02X} = {:02X}     ", addr, addr_x, data)?
                    }
                    None => write!(f, "${:02X},X @ {:02X} REG      ", addr, addr_x)?,
                }
            }
            ZPY => {
                let addr = self.try_loadb(self.regs.pc + 1).unwrap();
                let addr_y = addr.wrapping_add(self.regs.y) as u16;
                match self.try_loadb(addr_y) {
                    Some(data) => {
                        write!(f, "${:02X},Y @ {:02X} = {:02X}     ", addr, addr_y, data)?
                    }
                    None => write!(f, "${:02X},Y @ {:02X} REG      ", addr, addr_y)?,
                }
            }
            ZIX => {
                let addr = self.try_loadb(self.regs.pc + 1).unwrap();
                let addr_x = addr.wrapping_add(self.regs.x);
                let addr_zw = self.try_loadw_zp(addr_x).unwrap();
                match self.try_loadb(addr_zw) {
                    Some(data) => write!(
                        f,
                        "(${:02X},X)@{:02X}= {:04X}= {:02X}",
                        addr, addr_x, addr_zw, data
                    )?,
                    None => write!(f, "(${:02X},X)@{:02X}= {:04X} REG", addr, addr_x, addr_zw)?,
                }
            }
            ZIY | ziy => {
                let addr = self.try_loadb(self.regs.pc + 1).unwrap();
                let addr_zw = self.try_loadw_zp(addr).unwrap();
                let addr_y = addr_zw.wrapping_add(self.regs.y as u16);
                let data = self.try_loadb(addr_y).unwrap();
                match self.try_loadb(addr_y) {
                    Some(data) => write!(
                        f,
                        "(${:02X}),Y={:04X}@{:04X}={:02X}",
                        addr, addr_zw, addr_y, data
                    )?,
                    None => write!(f, "(${:02X}),Y={:04X}@{:04X}REG", addr, addr_zw, addr_y)?,
                }
            }
            IND => {
                let addr = self.try_loadw(self.regs.pc + 1).unwrap();;
                let low = self.try_loadb(addr).unwrap();
                let high = self
                    .try_loadb((addr & 0xFF00) | ((addr + 1) & 0xFF))
                    .unwrap();
                let target = low as u16 | (high as u16) << 8;
                write!(f, "(${:04X}) = {:04X}      ", addr, target)?
            }
            NON => write!(f, "NON                 ")?,
        }
        write!(
            f,
            " A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU:{},{} CYC:{}",
            self.regs.a,
            self.regs.x,
            self.regs.y,
            self.regs.p.bits(),
            self.regs.s,
            self.ppu.cycle,
            self.ppu.scanline,
            self.cycle
        )
    }
}
