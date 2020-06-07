use std::cell::RefCell;
use std::ops::IndexMut;
use std::rc::Rc;

#[derive(Debug, Copy, Clone)]
enum Flag {
    N = 0b10000000,
    V = 0b01000000,
    D = 0b00001000,
    I = 0b00000100,
    Z = 0b00000010,
    C = 0b00000001,
}

type Memory = dyn IndexMut<u16, Output=u8>;

struct Cpu {
    mem: Rc<RefCell<Memory>>,
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    flags: u8,
    cycles_left: isize,
}

impl Cpu {
    pub fn new(mem: Rc<RefCell<Memory>>) -> Self {
        Self {
            mem,
            a: 0x00,
            x: 0x00,
            y: 0x00,
            pc: 0x000,
            flags: 0b00000000,
            cycles_left: 0,
        }
    }

    pub fn tick(&mut self) {
        let mut mem = self.mem.borrow_mut();

        if self.cycles_left > 0 {
            self.cycles_left -= 1;
            return;
        }

        // We are in first cycle of operation
        self.cycles_left -= 1;

        let byte = mem[self.pc];
        self.pc += 1;

        match byte {
            0xA9 => {
                self.cycles_left += 2;

                self.a = mem[self.pc];
                self.pc += 1;

                if self.a == 0x00 {
                    self.flags |= Flag::Z as u8;
                }

                if self.a & 0b10000000 > 0 {
                    self.flags |= Flag::N as u8;
                }

                return;
            }
            0xA5 => {
                self.cycles_left += 3;

                let low = mem[self.pc];
                self.pc += 1;

                let addr = u16::from_le_bytes([low, 0x00]);
                self.a = mem[addr];

                if self.a == 0x00 {
                    self.flags |= Flag::Z as u8;
                }

                if self.a & 0b10000000 > 0 {
                    self.flags |= Flag::N as u8;
                }

                return;
            }
            0xB5 => {
                self.cycles_left += 4;

                let low = mem[self.pc];
                self.pc += 1;

                self.a = mem[u16::from_le_bytes([low + self.x, 0x00])];

                if self.a == 0x00 {
                    self.flags |= Flag::Z as u8;
                }

                if self.a & 0b10000000 > 0 {
                    self.flags |= Flag::N as u8;
                }


                return;
            }
            0xBD => {
                self.cycles_left += 4;

                let low = mem[self.pc];
                self.pc += 1;
                let (addr, page_crossed) = low.overflowing_add(self.x);

                let hi = mem[self.pc];
                self.pc += 1;

                let page = if page_crossed {
                    self.cycles_left += 1;
                    hi + 1
                } else {
                    hi
                };

                self.a = mem[u16::from_le_bytes([addr, page])];

                if self.a == 0x00 {
                    self.flags |= Flag::Z as u8;
                }

                if self.a & 0b10000000 > 0 {
                    self.flags |= Flag::N as u8;
                }

                return;
            }
            0xB9 => {
                self.cycles_left += 4;

                let low = mem[self.pc];
                self.pc += 1;
                let (addr, page_crossed) = low.overflowing_add(self.y);

                let hi = mem[self.pc];
                self.pc += 1;

                let page = if page_crossed {
                    self.cycles_left += 1;
                    hi + 1
                } else {
                    hi
                };

                self.a = mem[u16::from_le_bytes([addr, page])];

                if self.a == 0x00 {
                    self.flags |= Flag::Z as u8;
                }

                if self.a & 0b10000000 > 0 {
                    self.flags |= Flag::N as u8;
                }

                return;
            }
            0xAD => {
                self.cycles_left += 4;

                let low = mem[self.pc];
                self.pc += 1;

                let hi = mem[self.pc];
                self.pc += 1;

                self.a = mem[u16::from_le_bytes([low, hi])];

                if self.a == 0x00 {
                    self.flags |= Flag::Z as u8;
                }

                if self.a & 0b10000000 > 0 {
                    self.flags |= Flag::N as u8;
                }


                return;
            }
            0xA1 => {
                self.cycles_left += 6;

                let operand = mem[self.pc];
                self.pc += 1;

                let zpg_addr = u16::from_le_bytes([operand, 0x00]);
                let ind_addr = zpg_addr + self.x as u16;
                let eff_addr = u16::from_le_bytes(dbg!([mem[ind_addr + 1], mem[ind_addr]]));

                self.a = dbg!(mem[dbg!(eff_addr)]);

                if self.a == 0x00 {
                    self.flags |= Flag::Z as u8;
                }

                if self.a & 0b10000000 > 0 {
                    self.flags |= Flag::N as u8;
                }
            }
            0x8D => {
                self.cycles_left += 4;

                let low = mem[self.pc];
                self.pc += 1;

                let hi = mem[self.pc];
                self.pc += 1;

                mem[u16::from_le_bytes([low, hi])] = self.a;

                return;
            }
            _ => unimplemented!("Opcode 0x{:02X?} is not implemented", byte),
        }
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

        fn advance(&mut self, t: usize) -> &TestCase {
            for _ in 0..t {
                self.cpu.tick();
            }
            self
        }
    }

    #[test]
    fn lda_imm() {
        run(&[0xA9, 0x01]).advance(2).assert_reg(A, 0x01);
    }

    #[test]
    fn lda_imm_zero_flag() {
        run(&[0xA9, NEG_NUMBER]).advance(2).assert_flag(N, true);
        run(&[0xA9, POS_NUMBER]).advance(2).assert_flag(N, false);
    }

    #[test]
    fn lda_imm_negative_flag() {
        run(&[0xA9, ZERO]).advance(2).assert_flag(Z, true);
        run(&[0xA9, NON_ZERO]).advance(2).assert_flag(Z, false);
    }

    #[test]
    fn lda_abs() {
        run(&[0xAD, 0xFE, 0xCA]).with_mem(0xCAFE, &[0xFF]).advance(4).assert_reg(A, 0xFF);
    }

    #[test]
    fn lda_abs_negative_flag() {
        run(&[0xAD, 0xFE, 0xCA]).with_mem(0xCAFE, &[NEG_NUMBER]).advance(4).assert_flag(N, true);
        run(&[0xAD, 0xFE, 0xCA]).with_mem(0xCAFE, &[POS_NUMBER]).advance(4).assert_flag(N, false);
    }

    #[test]
    fn lda_abs_zero_flag() {
        run(&[0xAD, 0xFE, 0xCA]).with_mem(0xCAFE, &[ZERO]).advance(4).assert_flag(Z, true);
        run(&[0xAD, 0xFE, 0xCA]).with_mem(0xCAFE, &[NON_ZERO]).advance(4).assert_flag(Z, false);
    }

    #[test]
    fn lda_abs_x() {
        run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12).with_mem(0x1020 + 0x12, &[0xFF]).advance(4).assert_reg(A, 0xFF);
    }

    #[test]
    fn lda_abs_x_negative_flag() {
        run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12).with_mem(0x1020 + 0x12, &[NEG_NUMBER]).advance(4).assert_flag(N, true);
        run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12).with_mem(0x1020 + 0x12, &[POS_NUMBER]).advance(4).assert_flag(N, false);
    }

    #[test]
    fn lda_abs_x_zero_flag() {
        run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12).with_mem(0x1020 + 0x12, &[ZERO]).advance(4).assert_flag(Z, true);
        run(&[0xBD, 0x20, 0x10]).with_reg(X, 0x12).with_mem(0x1020 + 0x12, &[NON_ZERO]).advance(4).assert_flag(Z, false);
    }

    #[test]
    fn lda_abs_x_page_cross() {
        run(&[0xBD, 0xFF, 0x21]).with_reg(X, 0x01).with_mem(0x21FF + 0x01, &[0xAD]).advance(5).assert_reg(A, 0xAD);
    }

    #[test]
    fn lda_abs_y() {
        run(&[0xB9, 0x20, 0x10]).with_reg(Y, 0x12).with_mem(0x1020 + 0x0012, &[0xFF]).advance(4).assert_reg(A, 0xFF);
    }

    #[test]
    fn lda_abs_y_negative_flag() {
        run(&[0xB9, 0x20, 0x10]).with_reg(Y, 0x12).with_mem(0x1020 + 0x12, &[NEG_NUMBER]).advance(4).assert_flag(N, true);
        run(&[0xB9, 0x20, 0x10]).with_reg(Y, 0x12).with_mem(0x1020 + 0x12, &[POS_NUMBER]).advance(4).assert_flag(N, false);
    }

    #[test]
    fn lda_abs_y_zero_flag() {
        run(&[0xB9, 0x20, 0x10]).with_reg(Y, 0x12).with_mem(0x1020 + 0x12, &[ZERO]).advance(4).assert_flag(Z, true);
        run(&[0xB9, 0x20, 0x10]).with_reg(Y, 0x12).with_mem(0x1020 + 0x12, &[NON_ZERO]).advance(4).assert_flag(Z, false);
    }

    #[test]
    fn lda_abs_y_page_cross() {
        run(&[0xB9, 0xFF, 0x21]).with_reg(Y, 0x01).with_mem(0x21FF + 0x01, &[0xAD]).advance(5).assert_reg(A, 0xAD);
    }

    #[test]
    fn lda_zpg() {
        run(&[0xA5, 0xED]).with_mem(0x00ED, &[0xFF]).advance(3).assert_reg(A, 0xFF);
    }

    #[test]
    fn lda_zpg_negative_flag() {
        run(&[0xA5, 0xED]).with_mem(0x00ED, &[NEG_NUMBER]).advance(3).assert_flag(N, true);
        run(&[0xA5, 0xED]).with_mem(0x00ED, &[POS_NUMBER]).advance(3).assert_flag(N, false);
    }

    #[test]
    fn lda_zpg_zero_flag() {
        run(&[0xA5, 0xED]).with_mem(0x00ED, &[ZERO]).advance(3).assert_flag(Z, true);
        run(&[0xA5, 0xED]).with_mem(0x00ED, &[NON_ZERO]).advance(3).assert_flag(Z, false);
    }

    #[test]
    fn lda_zpg_x() {
        run(&[0xB5, 0xED]).with_reg(X, 0x11).with_mem(0x00ED + 0x11, &[0xFF]).advance(4).assert_reg(A, 0xFF);
    }

    #[test]
    fn lda_zpg_x_negative_flag() {
        run(&[0xB5, 0xED]).with_reg(X, 0x11).with_mem(0x00ED + 0x11, &[NEG_NUMBER]).advance(4).assert_flag(N, true);
        run(&[0xB5, 0xED]).with_reg(X, 0x11).with_mem(0x00ED + 0x11, &[POS_NUMBER]).advance(4).assert_flag(N, false);
    }

    #[test]
    fn lda_zpg_x_zero_flag() {
        run(&[0xB5, 0xED]).with_reg(X, 0x11).with_mem(0x00ED + 0x11, &[ZERO]).advance(4).assert_flag(Z, true);
        run(&[0xB5, 0xED]).with_reg(X, 0x11).with_mem(0x00ED + 0x11, &[NON_ZERO]).advance(4).assert_flag(Z, false);
    }

    #[test]
    fn lda_ind_x() {
        run(&[0xA1, 0xAB]).with_reg(X, 0x11).with_mem(0x00AB + 0x11, &[0xCA, 0xFE]).with_mem(0xCAFE, &[0xFF]).advance(6).assert_reg(A, 0xFF);
    }

    #[test]
    fn lda_ind_x_add_with_overflow() {
        run(&[0xA1, 0xFF]).with_reg(X, 0xFF).with_mem(0x00FF + 0xFF, &[0xCA, 0xFE]).with_mem(0xCAFE, &[0xFF]).advance(6).assert_reg(A, 0xFF);
    }

    #[test]
    fn lda_ind_x_negative_flag() {
        run(&[0xA1, 0xAB]).with_reg(X, 0x11).with_mem(0x00AB + 0x11, &[0xCA, 0xFE]).with_mem(0xCAFE, &[POS_NUMBER]).advance(6).assert_flag(N, false);
        run(&[0xA1, 0xAB]).with_reg(X, 0x11).with_mem(0x00AB + 0x11, &[0xCA, 0xFE]).with_mem(0xCAFE, &[NEG_NUMBER]).advance(6).assert_flag(N, true);
    }

    #[test]
    fn lda_ind_x_zero_flag() {
        run(&[0xA1, 0xAB]).with_reg(X, 0x11).with_mem(0x00AB + 0x11, &[0xCA, 0xFE]).with_mem(0xCAFE, &[ZERO]).advance(6).assert_flag(Z, true);
        run(&[0xA1, 0xAB]).with_reg(X, 0x11).with_mem(0x00AB + 0x11, &[0xCA, 0xFE]).with_mem(0xCAFE, &[NON_ZERO]).advance(6).assert_flag(Z, false);
    }

    #[test]
    fn sta_abs() {
        run(&[0x8D, 0x00, 0x02]).with_reg(A, 0x01).advance(4).assert_mem(0x0200, 0x01);
    }
}
