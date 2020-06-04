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
    use std::io::Write;

    use super::*;

    const NEG_NUMBER: u8 = 0x81;
    const POS_NUMBER: u8 = 0x01;
    const ZERO: u8 = 0x00;
    const NON_ZERO: u8 = 0x01;

    const MEM_START: u16 = 0x0000;

    struct TestMemory([u8; std::u16::MAX as usize]);

    impl TestMemory {
        fn new() -> Self {
            TestMemory([0x00; std::u16::MAX as usize])
        }

        fn write_bytes(&mut self, start: u16, bytes: &[u8]) {
            let _ = (&mut self.0[start as usize..]).write(bytes);
        }
    }

    enum Register { A, X, Y }

    impl Cpu<'_> {
        fn with_mem(mem: &mut TestMemory) -> Cpu {
            Cpu::new(&mut mem.0)
        }

        fn test_flag(&self, flag: Flag) -> bool {
            (self.flags & flag as u8) > 0
        }

        fn advance(&mut self, ticks: usize) {
            for _ in 0..ticks {
                self.tick();
            }
        }

        fn get_register(&self, reg: Register) -> u8 {
            match reg {
                Register::A => self.a,
                Register::X => self.x,
                Register::Y => self.y,
            }
        }

        fn set_register(&mut self, reg: Register, byte: u8) {
            match reg {
                Register::A => self.a = byte,
                Register::X => self.x = byte,
                Register::Y => self.y = byte,
            }
        }
    }

    #[test]
    fn lda_imm() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xA9, 0x01]); // LDA #$01

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(2);

        assert_eq!(cpu.get_register(Register::A), 0x01);
    }

    #[test]
    fn lda_imm_sets_negative_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xA9, NEG_NUMBER]); // LDA #$81

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(2);

        assert_eq!(cpu.test_flag(Flag::N), true)
    }

    #[test]
    fn lda_imm_sets_zero_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xA9, ZERO]); // LDA #$00

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(2);

        assert_eq!(cpu.test_flag(Flag::Z), true)
    }

    #[test]
    fn lda_imm_clears_negative_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xA9, POS_NUMBER]); // LDA #$xx

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(2);

        assert_eq!(cpu.test_flag(Flag::N), false)
    }

    #[test]
    fn lda_imm_clears_zero_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xA9, NON_ZERO]); // LDA #$01

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(2);

        assert_eq!(cpu.test_flag(Flag::Z), false)
    }

    #[test]
    fn lda_abs() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xAD, 0xFE, 0xCA]); // LDA $CAFE
        mem.write_bytes(0xCAFE, &[0xFF]);

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(4);

        assert_eq!(cpu.get_register(Register::A), 0xFF);
    }

    #[test]
    fn lda_abs_sets_negative_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xAD, 0xFE, 0xCA]); // LDA $CAFE
        mem.write_bytes(0xCAFE, &[NEG_NUMBER]);

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::N), true)
    }

    #[test]
    fn lda_abs_sets_zero_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xAD, 0xFE, 0xCA]); // LDA $CAFE
        mem.write_bytes(0xCAFE, &[ZERO]);

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::Z), true)
    }

    #[test]
    fn lda_abs_clears_negative_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xAD, 0xFE, 0xCA]); // LDA $CAFE
        mem.write_bytes(0xCAFE, &[POS_NUMBER]);

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::N), false)
    }

    #[test]
    fn lda_abs_clears_zero_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xAD, 0xFE, 0xCA]); // LDA $CAFE
        mem.write_bytes(0xCAFE, &[NON_ZERO]);

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::Z), false)
    }

    #[test]
    fn lda_abs_x() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xBD, 0x10, 0x02]); // LDA $0210,X
        mem.write_bytes(0x0210 + 0x12, &[0xAD]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::X, 0x12);

        cpu.advance(4);

        assert_eq!(cpu.get_register(Register::A), 0xAD);
    }

    #[test]
    fn lda_abs_x_sets_negative_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xBD, 0x10, 0x02]); // LDA $0210,X
        mem.write_bytes(0x0210 + 0x12, &[NEG_NUMBER]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::X, 0x12);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::N), true)
    }

    #[test]
    fn lda_abs_x_sets_zero_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xBD, 0x10, 0x02]); // LDA $0210,X
        mem.write_bytes(0x0210 + 0x12, &[ZERO]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::X, 0x12);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::Z), true)
    }

    #[test]
    fn lda_abs_x_clears_negative_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xBD, 0x10, 0x02]); // LDA $0210,X
        mem.write_bytes(0x0210 + 0x12, &[POS_NUMBER]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::X, 0x12);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::N), false)
    }

    #[test]
    fn lda_abs_x_clears_zero_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xBD, 0x10, 0x02]); // LDA $0210,X
        mem.write_bytes(0x0210 + 0x12, &[NON_ZERO]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::X, 0x12);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::Z), false)
    }

    #[test]
    fn lda_abs_x_page_cross() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xBD, 0xFF, 0x21]); // LDA $21FF,X
        mem.write_bytes(0x21FF + 0x01, &[0xAD]); // <-- page cross

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::X, 0x01);

        cpu.advance(5);

        assert_eq!(cpu.get_register(Register::A), 0xAD);
    }

    #[test]
    fn lda_abs_y() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xB9, 0x10, 0x02]); // LDA $0210,X
        mem.write_bytes(0x0210 + 0x12, &[0xAD]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::Y, 0x12);

        cpu.advance(4);

        assert_eq!(cpu.get_register(Register::A), 0xAD);
    }

    #[test]
    fn lda_abs_y_sets_negative_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xB9, 0x10, 0x02]); // LDA $0210,X
        mem.write_bytes(0x0210 + 0x12, &[NEG_NUMBER]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::Y, 0x12);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::N), true)
    }

    #[test]
    fn lda_abs_y_sets_zero_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xB9, 0x10, 0x02]); // LDA $0210,X
        mem.write_bytes(0x0210 + 0x12, &[ZERO]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::Y, 0x12);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::Z), true)
    }

    #[test]
    fn lda_abs_y_clears_negative_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xB9, 0x10, 0x02]); // LDA $0210,X
        mem.write_bytes(0x0210 + 0x12, &[POS_NUMBER]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::Y, 0x12);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::N), false)
    }

    #[test]
    fn lda_abs_y_clears_zero_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xB9, 0x10, 0x02]); // LDA $0210,X
        mem.write_bytes(0x0210 + 0x12, &[NON_ZERO]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::Y, 0x12);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::Z), false)
    }

    #[test]
    fn lda_abs_y_page_cross() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xB9, 0xFF, 0x21]); // LDA $21FF,X
        mem.write_bytes(0x21FF + 0x01, &[0xAD]); // <-- page cross

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::Y, 0x01);

        cpu.advance(5);

        assert_eq!(cpu.get_register(Register::A), 0xAD);
    }

    #[test]
    fn lda_zpg() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xA5, 0xED]); // LDA $ED
        mem.write_bytes(0x00ED, &[0xFE]);

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(3);

        assert_eq!(cpu.get_register(Register::A), 0xFE);
    }

    #[test]
    fn lda_zpg_sets_negative_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xA5, 0xED]); // LDA $ED
        mem.write_bytes(0x00ED, &[NEG_NUMBER]);

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(3);

        assert_eq!(cpu.test_flag(Flag::N), true)
    }

    #[test]
    fn lda_zpg_sets_zero_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xA5, 0xED]); // LDA $ED
        mem.write_bytes(0x00ED, &[ZERO]);

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(3);

        assert_eq!(cpu.test_flag(Flag::Z), true)
    }

    #[test]
    fn lda_zpg_clears_negative_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xA5, 0xED]); // LDA $ED
        mem.write_bytes(0x00ED, &[POS_NUMBER]);

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(3);

        assert_eq!(cpu.test_flag(Flag::N), false)
    }

    #[test]
    fn lda_zpg_clears_zero_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xA5, 0xED]); // LDA $ED
        mem.write_bytes(0x00ED, &[NON_ZERO]);

        let mut cpu = Cpu::with_mem(&mut mem);

        cpu.advance(3);

        assert_eq!(cpu.test_flag(Flag::Z), false)
    }

    #[test]
    fn lda_zpg_x() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xB5, 0xED]); // LDA $ED,X
        mem.write_bytes(0x00ED + 0x0011, &[0xCE]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::X, 0x11);

        cpu.advance(4);

        assert_eq!(cpu.get_register(Register::A), 0xCE);
    }

    #[test]
    fn lda_zpg_x_sets_negative_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xB5, 0xED]); // LDA $ED,X
        mem.write_bytes(0x00ED + 0x0011, &[NEG_NUMBER]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::X, 0x11);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::N), true)
    }

    #[test]
    fn lda_zpg_x_sets_zero_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xB5, 0xED]); // LDA $ED,X
        mem.write_bytes(0x00ED + 0x0011, &[ZERO]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::X, 0x11);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::Z), true)
    }

    #[test]
    fn lda_zpg_x_clears_negative_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xB5, 0xED]); // LDA $ED,X
        mem.write_bytes(0x00ED + 0x0011, &[POS_NUMBER]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::X, 0x11);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::N), false)
    }

    #[test]
    fn lda_zpg_x_clears_zero_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0xB5, 0xED]); // LDA $ED,X
        mem.write_bytes(0x00ED + 0x0011, &[NON_ZERO]);

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::X, 0x11);

        cpu.advance(4);

        assert_eq!(cpu.test_flag(Flag::Z), false)
    }

    #[test]
    fn sta_abs() {
        let mut mem = TestMemory::new();
        mem.write_bytes(MEM_START, &[0x8D, 0x00, 0x02]); // STA $0200

        let mut cpu = Cpu::with_mem(&mut mem);
        cpu.set_register(Register::A, 0x01);

        cpu.advance(4);

        assert_eq!(mem.0[0x0200], 0x01);
    }
}
