#[derive(Debug, Copy, Clone)]
enum Flag {
    N = 0b10000000,
    V = 0b01000000,
    D = 0b00001000,
    I = 0b00000100,
    Z = 0b00000010,
    C = 0b00000001,
}

struct Cpu<'a> {
    mem: &'a mut [u8],
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    flags: u8,
    cycles_left: isize,
}

impl<'a> Cpu<'a> {
    pub fn new(mem: &'a mut [u8]) -> Self {
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
        if self.cycles_left > 0 {
            self.cycles_left -= 1;
            return;
        }

        // We are in first cycle of operation
        self.cycles_left -= 1;

        let byte = self.mem[self.pc as usize];
        self.pc += 1;

        match byte {
            0xA9 => {
                self.cycles_left += 2;

                self.a = self.mem[self.pc as usize];
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

                let low = self.mem[self.pc as usize];
                self.pc += 1;

                let addr = u16::from_le_bytes([low, 0x00]);
                self.a = self.mem[addr as usize];

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

                let low = self.mem[self.pc as usize];
                self.pc += 1;

                self.a = self.mem[u16::from_le_bytes([low + self.x, 0x00]) as usize];

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

                let low = self.mem[self.pc as usize];
                self.pc += 1;
                let (addr, page_crossed) = low.overflowing_add(self.x);

                let hi = self.mem[self.pc as usize];
                self.pc += 1;

                let page = if page_crossed {
                    self.cycles_left += 1;
                    hi + 1
                } else {
                    hi
                };

                self.a = self.mem[u16::from_le_bytes([addr, page]) as usize];

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

                let low = self.mem[self.pc as usize];
                self.pc += 1;
                let (addr, page_crossed) = low.overflowing_add(self.y);

                let hi = self.mem[self.pc as usize];
                self.pc += 1;

                let page = if page_crossed {
                    self.cycles_left += 1;
                    hi + 1
                } else {
                    hi
                };

                self.a = self.mem[u16::from_le_bytes([addr, page]) as usize];

                if self.a == 0x00 {
                    self.flags |= Flag::Z as u8;
                }

                if self.a & 0b10000000 > 0 {
                    self.flags |= Flag::N as u8;
                }

                return;
            }
            0x8D => {
                self.cycles_left += 4;

                let low = self.mem[self.pc as usize];
                self.pc += 1;

                let hi = self.mem[self.pc as usize];
                self.pc += 1;

                self.mem[u16::from_le_bytes([low, hi]) as usize] = self.a;


                return;
            }
            0xAD => {
                self.cycles_left += 4;

                let low = self.mem[self.pc as usize];
                self.pc += 1;

                let hi = self.mem[self.pc as usize];
                self.pc += 1;

                self.a = self.mem[u16::from_le_bytes([low, hi]) as usize];

                if self.a == 0x00 {
                    self.flags |= Flag::Z as u8;
                }

                if self.a & 0b10000000 > 0 {
                    self.flags |= Flag::N as u8;
                }


                return;
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use std::io::Write;

    use Flag::*;
    use Register::*;

    use super::*;

    const NEG_NUMBER: u8 = 0x81;
    const POS_NUMBER: u8 = 0x01;
    const ZERO: u8 = 0x00;
    const NON_ZERO: u8 = 0x01;

    struct TestMemory([u8; std::u16::MAX as usize]);

    impl TestMemory {
        fn new() -> Self {
            TestMemory([0x00; std::u16::MAX as usize])
        }

        fn write_bytes(&mut self, start: u16, bytes: &[u8]) {
            let _ = (&mut self.0[start as usize..]).write(bytes);
        }
    }

    #[derive(Debug)]
    enum Register { A, X, Y }

    #[derive(Default)]
    struct CpuState {
        a: Option<u8>,
        x: Option<u8>,
        y: Option<u8>,
        flags: Option<u8>,
    }

    struct TestCase<'a> {
        program: Option<&'a [u8]>,
        mems: HashMap<u16, &'a [u8]>,
        init: CpuState,
        result: CpuState,
        result_mem: Vec<u8>,
    }

    impl<'a> TestCase<'a> {
        fn new(program: &'a [u8]) -> Self {
            Self {
                program: Some(program),
                mems: Default::default(),
                init: Default::default(),
                result: Default::default(),
                result_mem: Vec::new(),
            }
        }

        fn with_mem(&mut self, start: u16, bytes: &'a [u8]) -> &'a mut TestCase {
            self.mems.insert(start, bytes.clone());
            self
        }

        fn with_reg(&mut self, reg: Register, byte: u8) -> &'a mut TestCase {
            match reg {
                A => self.init.a = Some(byte),
                X => self.init.x = Some(byte),
                Y => self.init.y = Some(byte),
            }
            self
        }

        // fn with_flag(&mut self, flag: Flag, byte: u8) -> &'a mut T {
        //     unimplemented!();
        //     self
        // }

        fn assert_reg(&self, reg: Register, want: u8) -> &'a TestCase {
            let got = match reg {
                Register::A => self.result.a.unwrap(),
                Register::X => self.result.x.unwrap(),
                Register::Y => self.result.y.unwrap(),
            };
            assert_eq!(got, want, "{} value at register {:?} should be {:#02X?}, but was {:#02X?}", self.make_message(), reg, want, got);
            self
        }

        fn assert_flag(&self, flag: Flag, want: bool) -> &'a TestCase {
            let got = (self.result.flags.unwrap() & flag as u8) > 0;
            assert_eq!(got, want, "{} flag {:?} should be {}", self.make_message(), flag.clone(), if want { "set" } else { "unset" });
            self
        }

        fn assert_mem(&self, addr: u16, want: u8) -> &'a TestCase {
            let got = self.result_mem[addr as usize];
            assert_eq!(got, want, "{} memory at address {:#04X?} should be {:#02X?}, but was {:#02X?}", self.make_message(), addr, want, got);
            self
        }

        fn advance(&mut self, t: usize) -> &'a mut TestCase {
            let mut mem = TestMemory::new();
            mem.write_bytes(0x0000, self.program.expect("Program not defined"));

            for (start, bytes) in self.mems.iter() {
                mem.write_bytes(*start, bytes);
            }

            let mut cpu = Cpu::new(&mut mem.0);
            cpu.a = self.init.a.unwrap_or(0x00);
            cpu.x = self.init.x.unwrap_or(0x00);
            cpu.y = self.init.y.unwrap_or(0x00);
            cpu.flags = self.init.flags.unwrap_or(0x00);

            for _ in 0..t {
                cpu.tick();
            }

            self.result.a = Some(cpu.a);
            self.result.x = Some(cpu.x);
            self.result.y = Some(cpu.y);
            self.result.flags = Some(cpu.flags);
            let _ = self.result_mem.write_all(&mem.0);
            self
        }

        fn make_message(&self) -> String {
            format!("After running program {:02X?}", self.program.unwrap())
        }
    }

    #[test]
    fn lda_imm() {
        TestCase::new(&[0xA9, 0x01]).advance(2).assert_reg(A, 0x01);
        TestCase::new(&[0xA9, NEG_NUMBER]).advance(2).assert_flag(N, true);
        TestCase::new(&[0xA9, ZERO]).advance(2).assert_flag(Z, true);
        TestCase::new(&[0xA9, POS_NUMBER]).advance(2).assert_flag(N, false);
        TestCase::new(&[0xA9, NON_ZERO]).advance(2).assert_flag(Z, false);
    }

    #[test]
    fn lda_abs() {
        TestCase::new(&[0xAD, 0xFE, 0xCA]).with_mem(0xCAFE, &[0xFF]).advance(4).assert_reg(A, 0xFF);
        TestCase::new(&[0xAD, 0xFE, 0xCA]).with_mem(0xCAFE, &[NEG_NUMBER]).advance(4).assert_flag(N, true);
        TestCase::new(&[0xAD, 0xFE, 0xCA]).with_mem(0xCAFE, &[ZERO]).advance(4).assert_flag(Z, true);
        TestCase::new(&[0xAD, 0xFE, 0xCA]).with_mem(0xCAFE, &[POS_NUMBER]).advance(4).assert_flag(N, false);
        TestCase::new(&[0xAD, 0xFE, 0xCA]).with_mem(0xCAFE, &[NON_ZERO]).advance(4).assert_flag(Z, false);
    }

    #[test]
    fn lda_abs_x() {
        TestCase::new(&[0xBD, 0x10, 0x02]).with_mem(0x0210 + 0x12, &[0xFF]).with_reg(X, 0x12).advance(4).assert_reg(A, 0xFF);
        TestCase::new(&[0xBD, 0x10, 0x02]).with_mem(0x0210 + 0x12, &[NEG_NUMBER]).with_reg(X, 0x12).advance(4).assert_flag(N, true);
        TestCase::new(&[0xBD, 0x10, 0x02]).with_mem(0x0210 + 0x12, &[ZERO]).with_reg(X, 0x12).advance(4).assert_flag(Z, true);
        TestCase::new(&[0xBD, 0x10, 0x02]).with_mem(0x0210 + 0x12, &[POS_NUMBER]).with_reg(X, 0x12).advance(4).assert_flag(N, false);
        TestCase::new(&[0xBD, 0x10, 0x02]).with_mem(0x0210 + 0x12, &[NON_ZERO]).with_reg(X, 0x12).advance(4).assert_flag(Z, false);
        TestCase::new(&[0xBD, 0xFF, 0x21]).with_mem(0x21FF + 0x01, &[0xAD]).with_reg(X, 0x01).advance(5).assert_reg(A, 0xAD);
    }

    #[test]
    fn lda_abs_y() {
        TestCase::new(&[0xB9, 0x10, 0x02]).with_mem(0x0210 + 0x12, &[0xFF]).with_reg(Y, 0x12).advance(4).assert_reg(A, 0xFF);
        TestCase::new(&[0xB9, 0x10, 0x02]).with_mem(0x0210 + 0x12, &[NEG_NUMBER]).with_reg(Y, 0x12).advance(4).assert_flag(N, true);
        TestCase::new(&[0xB9, 0x10, 0x02]).with_mem(0x0210 + 0x12, &[ZERO]).with_reg(Y, 0x12).advance(4).assert_flag(Z, true);
        TestCase::new(&[0xB9, 0x10, 0x02]).with_mem(0x0210 + 0x12, &[POS_NUMBER]).with_reg(Y, 0x12).advance(4).assert_flag(N, false);
        TestCase::new(&[0xB9, 0x10, 0x02]).with_mem(0x0210 + 0x12, &[NON_ZERO]).with_reg(Y, 0x12).advance(4).assert_flag(Z, false);
        TestCase::new(&[0xB9, 0xFF, 0x21]).with_mem(0x21FF + 0x01, &[0xAD]).with_reg(Y, 0x01).advance(5).assert_reg(A, 0xAD);
    }

    #[test]
    fn lda_zpg() {
        TestCase::new(&[0xA5, 0xED]).with_mem(0x00ED, &[0xFF]).advance(3).assert_reg(A, 0xFF);
        TestCase::new(&[0xA5, 0xED]).with_mem(0x00ED, &[NEG_NUMBER]).advance(3).assert_flag(N, true);
        TestCase::new(&[0xA5, 0xED]).with_mem(0x00ED, &[ZERO]).advance(3).assert_flag(Z, true);
        TestCase::new(&[0xA5, 0xED]).with_mem(0x00ED, &[POS_NUMBER]).advance(3).assert_flag(N, false);
        TestCase::new(&[0xA5, 0xED]).with_mem(0x00ED, &[NON_ZERO]).advance(3).assert_flag(Z, false);
    }

    #[test]
    fn lda_zpg_x() {
        TestCase::new(&[0xB5, 0xED]).with_mem(0x00ED + 0x0011, &[0xFF]).with_reg(X, 0x11).advance(4).assert_reg(A, 0xFF);
        TestCase::new(&[0xB5, 0xED]).with_mem(0x00ED + 0x0011, &[NEG_NUMBER]).with_reg(X, 0x11).advance(4).assert_flag(N, true);
        TestCase::new(&[0xB5, 0xED]).with_mem(0x00ED + 0x0011, &[ZERO]).with_reg(X, 0x11).advance(4).assert_flag(Z, true);
        TestCase::new(&[0xB5, 0xED]).with_mem(0x00ED + 0x0011, &[POS_NUMBER]).with_reg(X, 0x11).advance(4).assert_flag(N, false);
        TestCase::new(&[0xB5, 0xED]).with_mem(0x00ED + 0x0011, &[NON_ZERO]).with_reg(X, 0x11).advance(4).assert_flag(Z, false);
    }

    #[test]
    fn sta_abs() {
        TestCase::new(&[0x8D, 0x00, 0x02]).with_reg(A, 0x01).advance(4).assert_mem(0x0200, 0x01);
    }
}
