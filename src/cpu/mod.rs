use std::cell::RefCell;
use std::ops::{IndexMut};
use std::rc::Rc;

type Flags = u8;
type Addr = u16;
type Memory = dyn IndexMut<Addr, Output=u8>;

#[derive(Debug, Copy, Clone)]
enum Flag {
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
    cycles_left: usize,
    ir: u8,
    addr_hi: u8,
    addr_lo: u8,
    data: u8,
    timer_state: u8,
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
            flags: 0b00000000,
            cycles_left: 0,
            ir: 0x00,
            addr_hi: 0x00,
            addr_lo: 0x00,
            data: 0x00,
            timer_state: 0,
            total_cycles: 0,
        }
    }

    pub fn tick(&mut self) {
        use Cmd::*;
        use AddrMode::*;

        self.total_cycles += 1;

        let (op, addr_mode) = (match &self.ir {
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

        match (op, addr_mode, &self.timer_state) {
            (_, _, 0) => {
                self.ir = self.fetch();
                self.timer_state = 1;
            }
            (LDA, Imm, 1) => {
                self.data = self.fetch();
                self.timer_state = 0;
                self.a = self.data;
                self.set_zero_flag(self.a);
                self.set_negative_flag(self.a);
            }
            (LDA, Zpg, 1) => {
                self.data = self.fetch();
                self.timer_state = 2;
            }
            (LDA, Zpg, 2) => {
                self.data = self.mem_read(u16::from_le_bytes([self.data, 0x00]));
                self.a = self.data;
                self.set_zero_flag(self.a);
                self.set_negative_flag(self.a);
                self.timer_state = 0;
            }
            (LDA, ZpgX, 1) => {
                self.data = self.fetch();
                self.timer_state = 2;
            }
            (LDA, ZpgX, 2) => {
                self.addr_lo = self.data.wrapping_add(self.x);
                self.timer_state = 3;
            }
            (LDA, ZpgX, 3) => {
                self.addr_hi = 0x00;
                self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                self.a = self.data;
                self.set_zero_flag(self.a);
                self.set_negative_flag(self.a);
                self.timer_state = 0;
            }
            (LDA, Abs, 1) => {
                self.data = self.fetch();
                self.timer_state = 2;
            }
            (LDA, Abs, 2) => {
                self.addr_hi = self.fetch();
                self.timer_state = 3;
            }
            (LDA, Abs, 3) => {
                self.addr_lo = self.data;
                self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                self.a = self.data;
                self.set_zero_flag(self.a);
                self.set_negative_flag(self.a);
                self.timer_state = 0;
            }
            (LDA, AbsX, 1) => {
                self.data = self.fetch();
                self.timer_state = 2;
            }
            (LDA, AbsX, 2) => {
                let (new_addr, page_crossed) = self.data.overflowing_add(self.x);
                self.addr_lo = new_addr;
                self.addr_hi = self.fetch();

                if page_crossed {
                    self.timer_state = 3;
                } else {
                    self.timer_state = 4;
                }
            }
            (LDA, AbsX, 3) => {
                self.addr_hi = self.addr_hi.wrapping_add(1);
                self.timer_state = 4;
            }
            (LDA, AbsX, 4) => {
                self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                self.a = self.data;
                self.set_zero_flag(self.a);
                self.set_negative_flag(self.a);
                self.timer_state = 0;
            }
            (LDA, AbsY, 1) => {
                self.data = self.fetch();
                self.timer_state = 2;
            }
            (LDA, AbsY, 2) => {
                let (new_addr, page_crossed) = self.data.overflowing_add(self.y);
                self.addr_lo = new_addr;
                self.addr_hi = self.fetch();

                if page_crossed {
                    self.timer_state = 3;
                } else {
                    self.timer_state = 4;
                }
            }
            (LDA, AbsY, 3) => {
                self.addr_hi = self.addr_hi.wrapping_add(1);
                self.timer_state = 4;
            }
            (LDA, AbsY, 4) => {
                self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                self.a = self.data;
                self.set_zero_flag(self.a);
                self.set_negative_flag(self.a);
                self.timer_state = 0;
            }
            (LDA, XInd, 1) => {
                self.data = self.fetch();
                self.timer_state = 2;
            }
            (LDA, XInd, 2) => {
                self.addr_lo = self.data.wrapping_add(self.x);
                self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, 0x00]));
                self.timer_state = 3;
            }
            (LDA, XInd, 3) => {
                self.addr_lo = self.addr_lo.wrapping_add(1);
                self.timer_state = 4;
            }
            (LDA, XInd, 4) => {
                self.addr_hi = self.mem_read(u16::from_le_bytes([self.addr_lo, 0x00]));
                self.timer_state = 5;
            }
            (LDA, XInd, 5) => {
                self.addr_lo = self.data;
                self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                self.a = self.data;
                self.set_zero_flag(self.a);
                self.set_negative_flag(self.a);
                self.timer_state = 0;
            }
            (LDA, IndY, 1) => {
                self.data = self.fetch();
                self.timer_state = 2;
            }
            (LDA, IndY, 2) => {
                self.addr_lo = self.data;
                self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, 0x00]));
                self.timer_state = 3;
            }
            (LDA, IndY, 3) => {
                self.addr_lo = self.addr_lo.wrapping_add(1);
                self.addr_hi = self.mem_read(u16::from_le_bytes([self.addr_lo, 0x00]));
                let (addr, page_crossed) = self.data.overflowing_add(self.y);
                self.addr_lo = addr;

                if page_crossed {
                    self.timer_state = 4;
                } else {
                    self.timer_state = 5;
                }
            }
            (LDA, IndY, 4) => {
                self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi.wrapping_add(1)]));
                self.a = self.data;

                self.set_zero_flag(self.a);
                self.set_negative_flag(self.a);
                self.timer_state = 5;
            }
            (LDA, IndY, 5) => {
                self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                self.a = self.data;

                self.set_zero_flag(self.a);
                self.set_negative_flag(self.a);

                self.timer_state = 0;
            }
            (LDX, Imm, 1) => {
                self.data = self.fetch();
                self.timer_state = 0;
                self.x = self.data;
                self.set_zero_flag(self.x);
                self.set_negative_flag(self.x);
            }
            (LDX, Zpg, 1) => {
                self.data = self.fetch();
                self.timer_state = 2;
            }
            (LDX, Zpg, 2) => {
                self.data = self.mem_read(u16::from_le_bytes([self.data, 0x00]));
                self.x = self.data;
                self.set_zero_flag(self.x);
                self.set_negative_flag(self.x);
                self.timer_state = 0;
            }
            (LDX, ZpgY, 1) => {
                self.data = self.fetch();
                self.timer_state = 2;
            }
            (LDX, ZpgY, 2) => {
                self.addr_lo = self.data.wrapping_add(self.y);
                self.timer_state = 3;
            }
            (LDX, ZpgY, 3) => {
                self.addr_hi = 0x00;
                self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                self.x = self.data;
                self.set_zero_flag(self.x);
                self.set_negative_flag(self.x);
                self.timer_state = 0;
            }
            (LDX, Abs, 1) => {
                self.data = self.fetch();
                self.timer_state = 2;
            }
            (LDX, Abs, 2) => {
                self.addr_hi = self.fetch();
                self.timer_state = 3;
            }
            (LDX, Abs, 3) => {
                self.addr_lo = self.data;
                self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                self.x = self.data;
                self.set_zero_flag(self.x);
                self.set_negative_flag(self.x);
                self.timer_state = 0;
            }
            (LDX, AbsY, 1) => {
                self.data = self.fetch();
                self.timer_state = 2;
            }
            (LDX, AbsY, 2) => {
                let (new_addr, page_crossed) = self.data.overflowing_add(self.y);
                self.addr_lo = new_addr;
                self.addr_hi = self.fetch();

                if page_crossed {
                    self.timer_state = 3;
                } else {
                    self.timer_state = 4;
                }
            }
            (LDX, AbsY, 3) => {
                self.addr_hi = self.addr_hi.wrapping_add(1);
                self.timer_state = 4;
            }
            (LDX, AbsY, 4) => {
                self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                self.x = self.data;
                self.set_zero_flag(self.x);
                self.set_negative_flag(self.x);
                self.timer_state = 0;
            }
            _ => unimplemented!()
        }
    }

    fn fetch(&mut self) -> u8 {
        let byte = self.mem_read(self.pc);
        self.pc += 1;
        byte
    }

    pub fn step(&mut self) {
        self.tick();
        self.tick();
        while self.timer_state != 1 {
            self.tick()
        }
    }

    fn set_addr_from_u16(&mut self, i: u16) {
        self.addr_lo = i.to_le_bytes()[0];
        self.addr_hi = i.to_le_bytes()[1];
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

    fn mem_read(&self, addr: Addr) -> u8 {
        self.mem.borrow()[addr]
    }
}

#[cfg(test)]
mod test {
    use std::io::Write;
    use std::ops::Index;

    use Flag::*;
    use Register::*;

    use super::*;

    const NEG_NUMBER: u8 = 0x81;
    const POS_NUMBER: u8 = 0x01;
    const ZERO: u8 = 0x00;
    const NON_ZERO: u8 = 0x01;

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
    enum Register { A, X, Y }

    struct TestCase {
        message: String,
        mem: Rc<RefCell<ArrayMemory>>,
        cpu: Cpu,
    }

    fn run(program: &[u8]) -> TestCase {
        let mem = Rc::new(RefCell::new(ArrayMemory([0x00; u16::MAX as usize])));
        let cpu = Cpu::new(mem.clone());

        let _ = (&mut RefCell::borrow_mut(&mem).0[0x0000..]).write_all(program);

        TestCase { message: format!("After running CPU with program {:02X?}", program), mem, cpu }
    }

    impl TestCase {
        fn with_mem(&mut self, start: u16, bytes: &[u8]) -> &mut TestCase {
            self.message = format!("{} and with memory {:02X?} at 0x{:02X?}", self.message, bytes, start);
            let _ = (&mut RefCell::borrow_mut(&self.mem).0[start as usize..]).write_all(bytes);
            self
        }

        fn with_reg(&mut self, reg: Register, byte: u8) -> &mut TestCase {
            self.message = format!("{} and with register {:?} set to 0x{:02X?}", self.message, reg, byte);
            match reg {
                A => self.cpu.a = byte,
                X => self.cpu.x = byte,
                Y => self.cpu.y = byte,
            }
            self
        }

        fn with_flag(&mut self, flag: Flag, val: bool) -> &mut TestCase {
            self.message = format!("{} and with flag {:?} {}", self.message, flag, if val { "set" } else { "unset" });
            self.cpu.flags ^= flag as u8;
            self
        }

        fn assert_reg(&self, reg: Register, want: u8) -> &TestCase {
            let got = match reg {
                Register::A => self.cpu.a,
                Register::X => self.cpu.x,
                Register::Y => self.cpu.y,
            };
            assert_eq!(got, want, "{} value at register {:?} should be 0x{:02X?}, but was 0x{:02X?}", self.message, reg, want, got);
            self
        }

        fn assert_flag(&self, flag: Flag, want: bool) -> &TestCase {
            let got = (self.cpu.flags & flag as u8) > 0;
            assert_eq!(got, want, "{} flag {:?} should be {}", self.message, flag.clone(), if want { "set" } else { "unset" });
            self
        }

        fn assert_mem(&self, addr: u16, want: u8) -> &TestCase {
            let got = RefCell::borrow(&self.mem)[addr];
            assert_eq!(got, want, "{} memory at address 0x{:04X?} should be 0x{:#02X?}, but was 0x{:02X?}", self.message, addr, want, got);
            self
        }

        fn assert_cycles(&self, want: usize) -> &TestCase {
            let got = self.cpu.total_cycles - 1; // Do not count initial cycle
            assert_eq!(got, want, "{} total cycles to be {}, but was {}", self.message, want, got);
            self
        }

        fn step(&mut self) -> &TestCase {
            self.cpu.step();
            self
        }
    }


    mod opcode;
}