use std::cell::RefCell;
use std::ops::{IndexMut};
use std::rc::Rc;

type Flags = u8;
type Addr = u16;
type Memory = dyn IndexMut<Addr, Output=u8>;

#[derive(Debug, Copy, Clone)]
pub enum Flag {
    N = 0b10000000,
    V = 0b01000000,
    D = 0b00001000,
    I = 0b00000100,
    Z = 0b00000010,
    C = 0b00000001,
}

#[derive(Debug, Copy, Clone)]
pub enum Cmd {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
}

#[derive(Debug, Copy, Clone)]
enum AddrMode {
    Acc,
    Imp,
    Imm,
    Zpg,
    ZpgX,
    ZpgY,
    Rel,
    Abs,
    AbsX,
    AbsY,
    Ind,
    XInd,
    IndY,
}

struct Cpu {
    mem: Rc<RefCell<Memory>>,
    a: u8,
    x: u8,
    y: u8,
    pc: Addr,
    flags: Flags,
    total_cycles: usize,
}

impl Cpu {
    pub fn new(mem: Rc<RefCell<Memory>>) -> Self {
        Self {
            mem,
            a: 0x00,
            x: 0x00,
            y: 0x00,
            pc: 0x0000,
            flags: 0b00100000,
            total_cycles: 0,
        }
    }

    pub fn step(&mut self) {
        use Cmd::*;
        use AddrMode::*;

        let byte = self.fetch();

        // Decode
        let (op, addr_mode) = (match &byte {
            0xA9 => Some((LDA, Imm)),
            0xA5 => Some((LDA, Zpg)),
            0xB5 => Some((LDA, ZpgX)),
            0xAD => Some((LDA, Abs)),
            0xBD => Some((LDA, AbsX)),
            0xB9 => Some((LDA, AbsY)),
            0xA1 => Some((LDA, XInd)),
            0xB1 => Some((LDA, IndY)),

            0xA2 => Some((LDX, Imm)),
            0xA6 => Some((LDX, Zpg)),
            0xB6 => Some((LDX, ZpgY)),
            0xAE => Some((LDX, Abs)),
            0xBE => Some((LDX, AbsY)),

            0x00 => Some((BRK, Imp)),
            _ => None
        }).unwrap();

        let operand = match addr_mode {
            Imp => 0xFF,
            Acc => self.a,
            Imm => self.fetch(),
            Zpg => {
                let zpg_addr = self.fetch();
                self.mem_read(u16::from_le_bytes([zpg_addr, 0x00]))
            }
            ZpgX => {
                let zpg_addr = self.fetch();
                self.inc();
                self.mem_read(u16::from_le_bytes([zpg_addr.wrapping_add(self.x), 0x00]))
            }
            ZpgY => {
                let zpg_addr = self.fetch();
                self.inc();
                self.mem_read(u16::from_le_bytes([zpg_addr.wrapping_add(self.y), 0x00]))
            }
            Abs => {
                let addr_lo = self.fetch();
                let addr_hi = self.fetch();
                self.mem_read(u16::from_le_bytes([addr_lo, addr_hi]))
            }
            AbsX => {
                let addr_lo = self.fetch();
                let addr_hi = self.fetch();

                let (eff_lo, carry) = addr_lo.overflowing_add(self.x);
                let addr_hi = if carry {
                    self.inc();
                    addr_hi + 1
                } else {
                    addr_hi
                };

                self.mem_read(u16::from_le_bytes([eff_lo, addr_hi]))
            }
            AbsY => {
                let addr_lo = self.fetch();
                let addr_hi = self.fetch();

                let (eff_lo, carry) = addr_lo.overflowing_add(self.y);
                let addr_hi = if carry {
                    self.inc();
                    addr_hi.wrapping_add(1)
                } else {
                    addr_hi
                };

                self.mem_read(u16::from_le_bytes([eff_lo, addr_hi]))
            }
            Ind => {
                let addr_lo = self.fetch();
                let addr_hi = self.fetch();

                let eff_lo = self.mem_read(u16::from_le_bytes([addr_lo, addr_hi]));
                let eff_hi = self.mem_read(u16::from_le_bytes([addr_lo, addr_hi]).wrapping_add(1));
                self.mem_read(u16::from_le_bytes([eff_lo, eff_hi]))
            }
            XInd => {
                let zpg_addr = self.fetch().wrapping_add(self.x);
                self.inc();

                let eff_lo = self.mem_read(u16::from_le_bytes([zpg_addr, 0x00]));
                let eff_hi = self.mem_read(u16::from_le_bytes([zpg_addr, 0x00]).wrapping_add(1));

                self.mem_read(u16::from_le_bytes([eff_lo, eff_hi]))
            }
            IndY => {
                let zpg_addr = self.fetch();

                let addr_lo = self.mem_read(u16::from_le_bytes([zpg_addr, 0x00]));
                let addr_hi = self.mem_read(u16::from_le_bytes([zpg_addr, 0x00]).wrapping_add(1));

                let (eff_lo, carry) = addr_lo.overflowing_add(self.y);
                let eff_hi = if carry {
                    self.inc();
                    addr_hi.wrapping_add(1)
                } else {
                    addr_hi
                };

                self.mem_read(u16::from_le_bytes([eff_lo, eff_hi]))
            }
            Rel => unimplemented!()
        };

        match op {
            ADC => {}
            AND => {}
            ASL => {}
            BCC => {}
            BCS => {}
            BEQ => {}
            BIT => {}
            BMI => {}
            BNE => {}
            BPL => {}
            BRK => {}
            BVC => {}
            BVS => {}
            CLC => {}
            CLD => {}
            CLI => {}
            CLV => {}
            CMP => {}
            CPX => {}
            CPY => {}
            DEC => {}
            DEX => {}
            DEY => {}
            EOR => {}
            INC => {}
            INX => {}
            INY => {}
            JMP => {}
            JSR => {}
            LDA => {
                self.a = operand;
                self.set_zero_flag(self.a);
                self.set_negative_flag(self.a);
            }
            LDX => {
                self.x = operand;
                self.set_zero_flag(self.x);
                self.set_negative_flag(self.x);
            }
            LDY => {}
            LSR => {}
            NOP => {}
            ORA => {}
            PHA => {}
            PHP => {}
            PLA => {}
            PLP => {}
            ROL => {}
            ROR => {}
            RTI => {}
            RTS => {}
            SBC => {}
            SEC => {}
            SED => {}
            SEI => {}
            STA => {}
            STX => {}
            STY => {}
            TAX => {}
            TAY => {}
            TSX => {}
            TXA => {}
            TXS => {}
            TYA => {}
        }
    }

    fn fetch(&mut self) -> u8 {
        let byte = self.mem_read(self.pc);
        self.pc += 1;
        byte
    }

    fn set_negative_flag(&mut self, i: u8) {
        if i & 0b10000000 > 0 {
            self.flags |= Flag::N as u8;
        } else {
            self.flags ^= Flag::N as u8;
        }
    }

    fn set_zero_flag(&mut self, i: u8) {
        if i == 0x00 {
            self.flags |= Flag::Z as u8;
        } else {
            self.flags ^= Flag::Z as u8;
        }
    }

    fn mem_read(&mut self, addr: Addr) -> u8 {
        self.inc();
        self.mem.borrow()[addr]
    }

    fn inc(&mut self) {
        self.total_cycles += 1;
    }
}

#[cfg(test)]
mod opcode_tests;

#[cfg(test)]
mod nestest;

#[cfg(test)]
mod test_helpers {
    use std::io::Write;
    use std::ops::Index;

    use Register::*;

    use super::*;

    pub const NEG_NUMBER: u8 = 0x81;
    pub const POS_NUMBER: u8 = 0x01;
    pub const ZERO: u8 = 0x00;
    pub const NON_ZERO: u8 = 0x01;

    struct ArrayMemory([u8; u16::MAX as usize]);

    impl Index<u16> for ArrayMemory {
        type Output = u8;

        fn index(&self, index: u16) -> &Self::Output {
            &self.0[index as usize]
        }
    }

    impl IndexMut<u16> for ArrayMemory {
        fn index_mut(&mut self, index: u16) -> &mut Self::Output {
            &mut self.0[index as usize]
        }
    }

    #[derive(Debug)]
    pub enum Register { A, X, Y }

    pub struct TestCase {
        message: String,
        mem: Rc<RefCell<ArrayMemory>>,
        cpu: Cpu,
    }

    pub fn run(program: &[u8]) -> TestCase {
        let mem = Rc::new(RefCell::new(ArrayMemory([0x00; u16::MAX as usize])));
        let cpu = Cpu::new(mem.clone());

        let _ = (&mut RefCell::borrow_mut(&mem).0[0x0000..]).write_all(program);

        TestCase { message: format!("After running CPU with program {:02X?}", program), mem, cpu }
    }

    impl TestCase {
        pub fn with_mem(&mut self, start: u16, bytes: &[u8]) -> &mut TestCase {
            self.message = format!("{} and with memory {:02X?} at 0x{:02X?}", self.message, bytes, start);
            let _ = (&mut RefCell::borrow_mut(&self.mem).0[start as usize..]).write_all(bytes);
            self
        }

        pub fn with_reg(&mut self, reg: Register, byte: u8) -> &mut TestCase {
            self.message = format!("{} and with register {:?} set to 0x{:02X?}", self.message, reg, byte);
            match reg {
                A => self.cpu.a = byte,
                X => self.cpu.x = byte,
                Y => self.cpu.y = byte,
            }
            self
        }

        pub fn with_flag(&mut self, flag: Flag, val: bool) -> &mut TestCase {
            self.message = format!("{} and with flag {:?} {}", self.message, flag, if val { "set" } else { "unset" });
            self.cpu.flags ^= flag as u8;
            self
        }

        pub fn assert_reg(&self, reg: Register, want: u8) -> &TestCase {
            let got = match reg {
                Register::A => self.cpu.a,
                Register::X => self.cpu.x,
                Register::Y => self.cpu.y,
            };
            assert_eq!(got, want, "{} value at register {:?} should be 0x{:02X?}, but was 0x{:02X?}", self.message, reg, want, got);
            self
        }

        pub fn assert_flag(&self, flag: Flag, want: bool) -> &TestCase {
            let got = (self.cpu.flags & flag as u8) > 0;
            assert_eq!(got, want, "{} flag {:?} should be {}", self.message, flag.clone(), if want { "set" } else { "unset" });
            self
        }

        pub fn assert_mem(&self, addr: u16, want: u8) -> &TestCase {
            let got = RefCell::borrow(&self.mem)[addr];
            assert_eq!(got, want, "{} memory at address 0x{:04X?} should be 0x{:#02X?}, but was 0x{:02X?}", self.message, addr, want, got);
            self
        }

        pub fn assert_cycles(&self, want: usize) -> &TestCase {
            let got = self.cpu.total_cycles;
            assert_eq!(got, want, "{} total cycles to be {}, but was {}", self.message, want, got);
            self
        }

        pub fn step(&mut self) -> &TestCase {
            self.cpu.step();
            self
        }
    }
}
