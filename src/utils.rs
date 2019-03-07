use crate::address::AM::{self, *};
use crate::{Memory, CPU};
impl<T: Memory> CPU<T> {
    pub fn loadb(&mut self, addr: u16) -> u8 {
        self.mem.loadb(addr)
    }
    pub fn loadw(&mut self, addr: u16) -> u16 {
        u16::from_le_bytes([self.loadb(addr), self.loadb(addr + 1)])
    }
    pub fn loadb_pc(&mut self) -> u8 {
        self.loadb(self.regs.pc)
    }
    pub fn loadw_pc(&mut self) -> u16 {
        self.loadw(self.regs.pc)
    }
    pub fn loadw_zp(&mut self, addr: u8) -> u16 {
        let low = self.loadb(addr as u16);
        let high = self.loadb(addr.wrapping_add(1) as u16);
        u16::from_le_bytes([low, high])
    }
    #[cfg(feature = "disasm")]
    pub fn try_loadb(&self, addr: u16) -> Option<u8> {
        self.mem.try_loadb(addr)
    }
    #[cfg(feature = "disasm")]
    pub fn try_loadw(&self, addr: u16) -> Option<u16> {
        Some(u16::from_le_bytes([
            self.try_loadb(addr)?,
            self.try_loadb(addr + 1)?,
        ]))
    }
    #[cfg(feature = "disasm")]
    pub fn try_loadw_zp(&self, addr: u8) -> Option<u16> {
        let low = self.try_loadb(addr as u16)?;
        let high = self.try_loadb(addr.wrapping_add(1) as u16)?;
        Some(u16::from_le_bytes([low, high]))
    }
    pub fn storeb(&mut self, addr: u16, val: u8) {
        self.mem.storeb(addr, val)
    }
    pub fn pushb(&mut self, val: u8) {
        let s = self.regs.s;
        self.regs.s = self.regs.s.wrapping_sub(1);
        self.storeb(0x100 + s as u16, val);
    }
    pub fn pushw(&mut self, val: u16) {
        self.pushb((val >> 8) as u8);
        self.pushb((val & 0xff) as u8)
    }
    pub fn popb(&mut self) -> u8 {
        self.regs.s = self.regs.s.wrapping_add(1);
        let s = self.regs.s;
        self.loadb(0x100 + s as u16)
    }
    pub fn popw(&mut self) -> u16 {
        let low = self.popb();
        let high = self.popb();
        u16::from_le_bytes([low, high])
    }
    pub fn load(&mut self, am: &AM) -> u8 {
        match *am {
            IMP => 0, //for NOP
            IMM => self.loadb_pc(),
            ACC => self.regs.a,
            _ => {
                let addr = self.get_addr(am);
                self.loadb(addr)
            }
        }
    }
    pub fn store(&mut self, am: &AM, val: u8) {
        match *am {
            ACC => self.regs.a = val,
            _ => {
                let addr = self.get_addr(am);
                self.storeb(addr, val);
            }
        }
    }
    pub fn branch(&mut self, flag: bool) -> u16 {
        let addr = self.loadb_pc();
        let old_pc = self.regs.pc + 1;
        if flag {
            let new_pc = ((addr as i8) as i32 + old_pc as i32) as u16;
            if ((new_pc ^ old_pc) >> 8) == 0 {
                self.mem.add_cycles(1);
            } else {
                self.mem.add_cycles(2);
            }
            new_pc
        } else {
            old_pc
        }
    }
    pub fn get_addr(&mut self, am: &AM) -> u16 {
        match *am {
            ABS => self.loadw_pc(),
            ZPG => self.loadb_pc() as u16,
            ABX => self.loadw_pc().wrapping_add(self.regs.x as u16),
            abx => {
                let base = self.loadw_pc();
                let addr = base.wrapping_add(self.regs.x as u16);
                if ((base ^ addr) >> 8) != 0 {
                    self.mem.add_cycles(1);
                }
                addr
            }
            ABY => self.loadw_pc().wrapping_add(self.regs.y as u16),
            aby => {
                let base = self.loadw_pc();
                let addr = base.wrapping_add(self.regs.y as u16);
                if ((base ^ addr) >> 8) != 0 {
                    self.mem.add_cycles(1);
                }
                addr
            }
            ZPX => self.loadb_pc().wrapping_add(self.regs.x) as u16,
            ZPY => self.loadb_pc().wrapping_add(self.regs.y) as u16,
            ZIX => {
                let addr = self.loadb_pc();
                self.loadw_zp(addr.wrapping_add(self.regs.x))
            }
            ZIY => {
                let addr = self.loadb_pc();
                self.loadw_zp(addr).wrapping_add(self.regs.y as u16)
            }
            ziy => {
                let addr = self.loadb_pc();
                let base = self.loadw_zp(addr);
                let addr = base.wrapping_add(self.regs.y as u16);
                if ((addr ^ base) >> 8) != 0 {
                    self.mem.add_cycles(1);
                }
                addr
            }
            IND => {
                let addr = self.loadw_pc();
                let low = self.loadb(addr);
                let high = self.loadb((addr & 0xFF00) | ((addr + 1) & 0xFF));
                low as u16 | (high as u16) << 8
            }
            _ => unimplemented!(),
        }
    }
}
