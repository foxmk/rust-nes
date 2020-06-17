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
        self.total_cycles += 1;

        let (op, addr_mode, _, _) = (match &self.ir {
            0xA9 => Some((Cmd::LDA, AddrMode::Imm, 2, 2)),
            0xA5 => Some((Cmd::LDA, AddrMode::Zpg, 2, 3)),
            0xB5 => Some((Cmd::LDA, AddrMode::ZpgX, 2, 4)),
            0xAD => Some((Cmd::LDA, AddrMode::Abs, 3, 4)),
            0xBD => Some((Cmd::LDA, AddrMode::AbsX, 3, 4)),
            0xB9 => Some((Cmd::LDA, AddrMode::AbsY, 3, 4)),
            0xA1 => Some((Cmd::LDA, AddrMode::XInd, 2, 6)),
            0xB1 => Some((Cmd::LDA, AddrMode::IndY, 2, 5)),
            0x00 => Some((Cmd::BRK, AddrMode::Imp, 0, 0)),
            _ => None
        }).unwrap();

        match (op, addr_mode) {
            (Cmd::LDA, AddrMode::Imm) => {
                match &self.timer_state {
                    1 => {
                        self.data = self.fetch();
                        self.timer_state = 0;
                    }
                    0 => {
                        self.a = self.data;
                        self.set_zero_flag(self.a);
                        self.set_negative_flag(self.a);

                        self.ir = self.fetch();
                        self.timer_state = 1;
                    }
                    _ => unreachable!()
                }
            }
            (Cmd::BRK, _) => {
                match &self.timer_state {
                    0 => {
                        self.ir = self.fetch();
                        self.timer_state = 1;
                    }
                    _ => unreachable!()
                }
            }
            (Cmd::LDA, AddrMode::Zpg) => {
                match &self.timer_state {
                    1 => {
                        self.data = self.fetch();
                        self.timer_state = 2;
                    }
                    2 => {
                        self.data = self.mem_read(u16::from_le_bytes([self.data, 0x00]));
                        self.timer_state = 0;
                    }
                    0 => {
                        self.a = self.data;
                        self.set_zero_flag(self.a);
                        self.set_negative_flag(self.a);

                        self.ir = self.fetch();
                        self.timer_state = 1;
                    }
                    _ => unreachable!()
                }
            }
            (Cmd::LDA, AddrMode::ZpgX) => {
                match &self.timer_state {
                    1 => {
                        self.data = self.fetch();
                        self.timer_state = 2;
                    }
                    2 => {
                        self.addr_lo = self.data.wrapping_add(self.x);
                        self.timer_state = 3;
                    }
                    3 => {
                        self.addr_hi = 0x00;
                        self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                        self.timer_state = 0;
                    }
                    0 => {
                        self.a = self.data;
                        self.set_zero_flag(self.a);
                        self.set_negative_flag(self.a);

                        self.ir = self.fetch();
                        self.timer_state = 1;
                    }
                    _ => unreachable!()
                }
            }
            (Cmd::LDA, AddrMode::Abs) => {
                match &self.timer_state {
                    1 => {
                        self.data = self.fetch();
                        self.timer_state = 2;
                    }
                    2 => {
                        self.addr_hi = self.fetch();
                        self.timer_state = 3;
                    }
                    3 => {
                        self.addr_lo = self.data;
                        self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                        self.timer_state = 0;
                    }
                    0 => {
                        self.a = self.data;
                        self.set_zero_flag(self.a);
                        self.set_negative_flag(self.a);

                        self.ir = self.fetch();
                        self.timer_state = 1;
                    }
                    _ => unreachable!()
                }
            }
            (Cmd::LDA, AddrMode::AbsX) => {
                match &self.timer_state {
                    1 => {
                        self.data = self.fetch();
                        self.timer_state = 2;
                    }
                    2 => {
                        let (new_addr, page_crossed) = self.data.overflowing_add(self.x);
                        self.addr_lo = new_addr;
                        self.addr_hi = self.fetch();

                        if page_crossed {
                            self.timer_state = 3;
                        } else {
                            self.timer_state = 4;
                        }
                    }
                    3 => {
                        self.addr_hi = self.addr_hi.wrapping_add(1);
                        self.timer_state = 4;
                    }
                    4 => {
                        self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                        self.timer_state = 0;
                    }
                    0 => {
                        self.a = self.data;
                        self.set_zero_flag(self.a);
                        self.set_negative_flag(self.a);

                        self.ir = self.fetch();
                        self.timer_state = 1;
                    }
                    _ => unreachable!()
                }
            }
            (Cmd::LDA, AddrMode::AbsY) => {
                match &self.timer_state {
                    1 => {
                        self.data = self.fetch();
                        self.timer_state = 2;
                    }
                    2 => {
                        let (new_addr, page_crossed) = self.data.overflowing_add(self.y);
                        self.addr_lo = new_addr;
                        self.addr_hi = self.fetch();

                        if page_crossed {
                            self.timer_state = 3;
                        } else {
                            self.timer_state = 4;
                        }
                    }
                    3 => {
                        self.addr_hi = self.addr_hi.wrapping_add(1);
                        self.timer_state = 4;
                    }
                    4 => {
                        self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                        self.timer_state = 0;
                    }
                    0 => {
                        self.a = self.data;
                        self.set_zero_flag(self.a);
                        self.set_negative_flag(self.a);

                        self.ir = self.fetch();
                        self.timer_state = 1;
                    }
                    _ => unreachable!()
                }
            }
            (Cmd::LDA, AddrMode::XInd) => {
                match &self.timer_state {
                    1 => {
                        self.data = self.fetch();
                        self.timer_state = 2;
                    }
                    2 => {
                        self.addr_lo = self.data.wrapping_add(self.x);
                        self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, 0x00]));
                        self.timer_state = 3;
                    }
                    3 => {
                        self.addr_lo = self.addr_lo.wrapping_add(1);
                        self.timer_state = 4;
                    }
                    4 => {
                        self.addr_hi = self.mem_read(u16::from_le_bytes([self.addr_lo, 0x00]));
                        self.timer_state = 5;
                    }
                    5 => {
                        self.addr_lo = self.data;
                        self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                        self.timer_state = 0;
                    }
                    0 => {
                        self.a = self.data;
                        self.set_zero_flag(self.a);
                        self.set_negative_flag(self.a);

                        self.ir = self.fetch();
                        self.timer_state = 1;
                    }
                    _ => unreachable!()
                }
            }
            (Cmd::LDA, AddrMode::IndY) => {
                match &self.timer_state {
                    1 => {
                        self.data = self.fetch();
                        self.timer_state = 2;
                    }
                    2 => {
                        self.addr_lo = self.data;
                        self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, 0x00]));
                        self.timer_state = 3;
                    }
                    3 => {
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
                    4 => {
                        self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi.wrapping_add(1)]));
                        self.a = self.data;

                        self.set_zero_flag(self.a);
                        self.set_negative_flag(self.a);
                        self.timer_state = 5;
                    }
                    5 => {
                        self.data = self.mem_read(u16::from_le_bytes([self.addr_lo, self.addr_hi]));
                        self.a = self.data;

                        self.set_zero_flag(self.a);
                        self.set_negative_flag(self.a);

                        self.timer_state = 0;
                    }
                    0 => {
                        self.ir = self.fetch();
                        self.timer_state = 1;
                    }
                    _ => unreachable!()
                }
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


    mod lda {
        use super::*;

        mod imm {
            use super::*;

            #[test]
            fn should_set_register_to_imm_value() {
                run(&[0xA9, 0x01])
                    .step()
                    .assert_reg(A, 0x01);
            }

            #[test]
            fn should_take_2_cycles() {
                run(&[0xA9, 0x01])
                    .step()
                    .assert_cycles(2);
            }

            #[test]
            fn should_set_negative_flag() {
                run(&[0xA9, NEG_NUMBER]).with_flag(N, false)
                    .step()
                    .assert_flag(N, true);
            }

            #[test]
            fn should_clear_negative_flag() {
                run(&[0xA9, POS_NUMBER]).with_flag(N, true)
                    .step()
                    .assert_flag(N, false);
            }

            #[test]
            fn should_set_zero_flag() {
                run(&[0xA9, ZERO]).with_flag(Z, false)
                    .step()
                    .assert_flag(Z, true);
            }

            #[test]
            fn should_clear_zero_flag() {
                run(&[0xA9, NON_ZERO]).with_flag(Z, true)
                    .step()
                    .assert_flag(Z, false);
            }
        }

        mod abs {
            use super::*;

            #[test]
            fn should_load_value_from_addr() {
                run(&[0xAD, 0xFE, 0xCA])
                    .with_mem(0xCAFE, &[0xFF])
                    .step()
                    .assert_reg(A, 0xFF);
            }

            #[test]
            fn should_take_4_cycles() {
                run(&[0xAD, 0xFE, 0xCA])
                    .with_mem(0xCAFE, &[0xFF])
                    .step()
                    .assert_cycles(4);
            }

            #[test]
            fn should_set_negative_flag() {
                run(&[0xAD, 0xFE, 0xCA]).with_flag(N, false)
                    .with_mem(0xCAFE, &[NEG_NUMBER])
                    .step()
                    .assert_flag(N, true);
            }

            #[test]
            fn should_clear_negative_flag() {
                run(&[0xAD, 0xFE, 0xCA]).with_flag(N, true)
                    .with_mem(0xCAFE, &[POS_NUMBER])
                    .step()
                    .assert_flag(N, false);
            }

            #[test]
            fn should_set_zero_flag() {
                run(&[0xAD, 0xFE, 0xCA]).with_flag(Z, false)
                    .with_mem(0xCAFE, &[ZERO])
                    .step()
                    .assert_flag(Z, true);
            }

            #[test]
            fn should_clear_zero_flag() {
                run(&[0xAD, 0xFE, 0xCA]).with_flag(Z, true)
                    .with_mem(0xCAFE, &[NON_ZERO])
                    .step()
                    .assert_flag(Z, false);
            }
        }

        mod abs_x {
            use super::*;

            #[test]
            fn should_load_a_from_addr_indexed_by_x() {
                run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12)
                    .with_mem(0x1020 + 0x12, &[0xFF])
                    .step()
                    .assert_reg(A, 0xFF);
            }

            #[test]
            fn should_take_4_cycles() {
                run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12)
                    .with_mem(0x1020 + 0x12, &[0xFF])
                    .step()
                    .assert_cycles(4);
            }

            #[test]
            fn should_set_negative_flag() {
                run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12).with_flag(N, false)
                    .with_mem(0x1020 + 0x12, &[NEG_NUMBER])
                    .step()
                    .assert_flag(N, true);
            }

            #[test]
            fn should_clear_negative_flag() {
                run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12).with_flag(N, true)
                    .with_mem(0x1020 + 0x12, &[POS_NUMBER])
                    .step()
                    .assert_flag(N, false);
            }

            #[test]
            fn should_set_zero_flag() {
                run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12).with_flag(Z, false)
                    .with_mem(0x1020 + 0x12, &[ZERO])
                    .step()
                    .assert_flag(Z, true);
            }

            #[test]
            fn should_clear_zero_flag() {
                run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12).with_flag(Z, true)
                    .with_mem(0x1020 + 0x12, &[NON_ZERO])
                    .step()
                    .assert_flag(Z, false);
            }

            #[test]
            fn should_add_cycle_on_page_cross() {
                run(&[0xBD, 0xFF, 0x21]).with_reg(X, 0x01)
                    .with_mem(0x21FF + 0x01, &[0xAD])
                    .step()
                    .assert_reg(A, 0xAD);
            }
        }

        mod abs_y {
            use super::*;

            #[test]
            fn should_load_a_from_addr_indexed_by_y() {
                run(&[0xB9, 0x20, 0x10]).with_reg(Y, 0x12)
                    .with_mem(0x1020 + 0x0012, &[0xFF])
                    .step()
                    .assert_reg(A, 0xFF);
            }

            #[test]
            fn should_take_4_cycles() {
                run(&[0xB9, 0x20, 0x10]).with_reg(Y, 0x12)
                    .with_mem(0x1020 + 0x0012, &[0xFF])
                    .step()
                    .assert_cycles(4);
            }

            #[test]
            fn should_set_negative_flag() {
                run(&[0xB9, 0x20, 0x10]).with_reg(Y, 0x12).with_flag(N, false)
                    .with_mem(0x1020 + 0x12, &[NEG_NUMBER])
                    .step()
                    .assert_flag(N, true);
            }

            #[test]
            fn should_clear_negative_flag() {
                run(&[0xB9, 0x20, 0x10]).with_reg(Y, 0x12).with_flag(N, true)
                    .with_mem(0x1020 + 0x12, &[POS_NUMBER])
                    .step()
                    .assert_flag(N, false);
            }

            #[test]
            fn should_set_zero_flag() {
                run(&[0xB9, 0x20, 0x10]).with_flag(Z, false).with_reg(Y, 0x12)
                    .with_mem(0x1020 + 0x12, &[ZERO])
                    .step()
                    .assert_flag(Z, true);
            }

            #[test]
            fn should_clear_zero_flag() {
                run(&[0xB9, 0x20, 0x10]).with_flag(Z, true).with_reg(Y, 0x12)
                    .with_mem(0x1020 + 0x12, &[NON_ZERO])
                    .step()
                    .assert_flag(Z, false);
            }

            #[test]
            fn should_add_cycle_on_page_cross() {
                run(&[0xB9, 0xFF, 0x21]).with_reg(Y, 0x01)
                    .with_mem(0x21FF + 0x01, &[0xAD])
                    .step()
                    .assert_cycles(5);
            }
        }

        mod zpg {
            use super::*;

            #[test]
            fn should_load_a_from_zpg_addr() {
                run(&[0xA5, 0xED]).with_mem(0x00ED, &[0xFF])
                    .step()
                    .assert_reg(A, 0xFF);
            }

            #[test]
            fn should_take_3_cycles() {
                run(&[0xA5, 0xED]).with_mem(0x00ED, &[0xFF])
                    .step()
                    .assert_cycles(3);
            }

            #[test]
            fn should_set_negative_flag() {
                run(&[0xA5, 0xED]).with_flag(N, false)
                    .with_mem(0x00ED, &[NEG_NUMBER])
                    .step()
                    .assert_flag(N, true);
            }

            #[test]
            fn should_clear_negative_flag() {
                run(&[0xA5, 0xED]).with_flag(N, true)
                    .with_mem(0x00ED, &[POS_NUMBER])
                    .step()
                    .assert_flag(N, false);
            }

            #[test]
            fn should_set_zero_flag() {
                run(&[0xA5, 0xED]).with_flag(Z, false)
                    .with_mem(0x00ED, &[ZERO])
                    .step()
                    .assert_flag(Z, true);
            }

            #[test]
            fn should_clear_zero_flag() {
                run(&[0xA5, 0xED]).with_flag(Z, true)
                    .with_mem(0x00ED, &[NON_ZERO])
                    .step()
                    .assert_flag(Z, false);
            }
        }

        mod zpg_x {
            use super::*;

            #[test]
            fn should_load_a_from_addr_indexed_by_x() {
                run(&[0xB5, 0xED]).with_reg(X, 0x11)
                    .with_mem(0x00ED + 0x11, &[0xFF])
                    .step()
                    .assert_reg(A, 0xFF);
            }

            #[test]
            fn should_take_4_cycles() {
                run(&[0xB5, 0xED]).with_reg(X, 0x11)
                    .with_mem(0x00ED + 0x11, &[0xFF])
                    .step()
                    .assert_cycles(4);
            }

            #[test]
            fn should_set_negative_flag() {
                run(&[0xB5, 0xED]).with_flag(N, false).with_reg(X, 0x11)
                    .with_mem(0x00ED + 0x11, &[NEG_NUMBER])
                    .step()
                    .assert_flag(N, true);
            }

            #[test]
            fn should_clear_negative_flag() {
                run(&[0xB5, 0xED]).with_flag(N, true).with_reg(X, 0x11)
                    .with_mem(0x00ED + 0x11, &[POS_NUMBER])
                    .step()
                    .assert_flag(N, false);
            }

            #[test]
            fn should_set_zero_flag() {
                run(&[0xB5, 0xED]).with_reg(X, 0x11).with_flag(Z, false)
                    .with_mem(0x00ED + 0x11, &[ZERO])
                    .step()
                    .assert_flag(Z, true);
            }

            #[test]
            fn should_clear_zero_flag() {
                run(&[0xB5, 0xED]).with_reg(X, 0x11).with_flag(Z, true)
                    .with_mem(0x00ED + 0x11, &[NON_ZERO])
                    .step()
                    .assert_flag(Z, false);
            }
        }

        mod x_ind {
            use super::*;

            #[test]
            fn should_load_a_from_addr_pointed_by_indirect_pre_indexed_by_x() {
                run(&[0xA1, 0xED]).with_reg(X, 0x01)
                    .with_mem(0x00ED + 0x01, &[0xFE, 0xCA]).with_mem(0xCAFE, &[0xFF])
                    .step()
                    .assert_reg(A, 0xFF);
            }

            #[test]
            fn should_take_6_cycles() {
                run(&[0xA1, 0xED]).with_reg(X, 0x01)
                    .with_mem(0x00ED + 0x01, &[0xFE, 0xCA]).with_mem(0xCAFE, &[0xFF])
                    .step()
                    .assert_cycles(6);
            }

            #[ignore]
            #[test]
            fn should_not_overflow() {
                run(&[0xA1, 0xFF]).with_reg(X, 0xFF)
                    .with_mem(0x00FF + 0xFF, &[0xFE, 0xCA]).with_mem(0xCAFE, &[0xFF])
                    .step()
                    .assert_reg(A, 0xFF);
            }

            #[test]
            fn should_clear_negative_flag() {
                run(&[0xA1, 0xED]).with_reg(X, 0x01).with_flag(N, true)
                    .with_mem(0x00ED + 0x01, &[0xFE, 0xCA]).with_mem(0xCAFE, &[POS_NUMBER])
                    .step()
                    .assert_flag(N, false);
            }

            #[test]
            fn should_set_negative_flag() {
                run(&[0xA1, 0xED]).with_reg(X, 0x01).with_flag(N, false)
                    .with_mem(0x00ED + 0x01, &[0xFE, 0xCA]).with_mem(0xCAFE, &[NEG_NUMBER])
                    .step()
                    .assert_flag(N, true);
            }

            #[test]
            fn should_set_zero_flag() {
                run(&[0xA1, 0xED]).with_reg(X, 0x01).with_flag(Z, false)
                    .with_mem(0x00ED + 0x01, &[0xFE, 0xCA]).with_mem(0xCAFE, &[ZERO])
                    .step()
                    .assert_flag(Z, true);
            }

            #[test]
            fn should_clear_zero_flag() {
                run(&[0xA1, 0xED]).with_reg(X, 0x01).with_flag(Z, true)
                    .with_mem(0x00ED + 0x01, &[0xFE, 0xCA]).with_mem(0xCAFE, &[NON_ZERO])
                    .step()
                    .assert_flag(Z, false);
            }
        }

        mod ind_y {
            use super::*;

            #[test]
            fn should_load_a_from_addr_pointed_by_indirect_post_indexed_by_y() {
                run(&[0xB1, 0xED]).with_reg(Y, 0x01)
                    .with_mem(0x00ED, &[0xFE, 0xCA]).with_mem(0xCAFE + 0x01, &[0xFF])
                    .step()
                    .assert_reg(A, 0xFF);
            }

            #[test]
            fn should_take_5_cycles() {
                run(&[0xB1, 0xED]).with_reg(Y, 0x01)
                    .with_mem(0x00ED, &[0xFE, 0xCA]).with_mem(0xCAFE + 0x01, &[0xFF])
                    .step()
                    .assert_cycles(5);
            }

            #[ignore]
            #[test]
            fn should_overflow() {
                run(&[0xB1, 0xFF]).with_reg(Y, 0xFF)
                    .with_mem(0x00FF, &[0xFE, 0xCA]).with_mem(0xCAFE + 0xFF, &[0xFF])
                    .step()
                    .assert_reg(A, 0xFF);
            }

            #[test]
            fn should_set_negative_flag() {
                run(&[0xB1, 0xED]).with_reg(Y, 0x01)
                    .with_flag(N, true).with_mem(0x00ED, &[0xFE, 0xCA]).with_mem(0xCAFE + 0x01, &[POS_NUMBER])
                    .step()
                    .assert_flag(N, false);
            }

            #[test]
            fn should_clear_negative_flag() {
                run(&[0xB1, 0xED]).with_reg(Y, 0x01).with_flag(N, false)
                    .with_mem(0x00ED, &[0xFE, 0xCA]).with_mem(0xCAFE + 0x01, &[NEG_NUMBER])
                    .step()
                    .assert_flag(N, true);
            }

            #[test]
            fn should_set_zero_flag() {
                run(&[0xB1, 0xED]).with_reg(Y, 0x01).with_flag(Z, false)
                    .with_mem(0x00ED, &[0xFE, 0xCA]).with_mem(0xCAFE + 0x01, &[ZERO])
                    .step()
                    .assert_flag(Z, true);
            }

            #[test]
            fn should_clear_zero_flag() {
                run(&[0xB1, 0xED]).with_reg(Y, 0x01).with_flag(Z, true)
                    .with_mem(0x00ED, &[0xFE, 0xCA]).with_mem(0xCAFE + 0x01, &[NON_ZERO])
                    .step()
                    .assert_flag(Z, false);
            }
        }
    }
}
