struct Cpu<'a> {
    mem: &'a mut [u8],
    a: u8,
    x: u8,
    pc: u16,
}

impl<'a> Cpu<'a> {
    pub fn new(mem: &'a mut [u8]) -> Self {
        Self {
            mem,
            a: 0x00,
            x: 0x00,
            pc: 0x0000,
        }
    }

    pub fn tick(&mut self, mut ticks: usize) {
        while ticks > 0 {
            let byte = self.mem[self.pc as usize];
            self.pc += 1;

            match byte {
                0xA9 => {
                    self.a = self.mem[self.pc as usize];
                    self.pc += 1;
                    ticks -= 2;
                    continue;
                }
                0xA5 => {
                    let low = self.mem[self.pc as usize];
                    self.pc += 1;

                    let addr = u16::from_le_bytes([low, 0x00]);
                    self.a = self.mem[addr as usize];

                    ticks -= 3;

                    continue;
                }
                0xB5 => {
                    let low = self.mem[self.pc as usize];
                    self.pc += 1;

                    self.a = self.mem[u16::from_le_bytes([low + self.x, 0x00]) as usize];

                    ticks -= 4;

                    continue;
                }
                0xBD => {
                    let low = self.mem[self.pc as usize];
                    self.pc += 1;
                    let (addr, page_crossed) = low.overflowing_add(self.x);

                    let hi = self.mem[self.pc as usize];
                    self.pc += 1;

                    let page = if page_crossed {
                        ticks -= 5;
                        hi + 1
                    } else {
                        ticks -= 4;
                        hi
                    };

                    self.a = self.mem[u16::from_le_bytes([addr, page]) as usize];

                    continue;
                }
                0x8D => {
                    let low = self.mem[self.pc as usize];
                    self.pc += 1;

                    let hi = self.mem[self.pc as usize];
                    self.pc += 1;

                    self.mem[u16::from_le_bytes([low, hi]) as usize] = self.a;

                    ticks -= 4;

                    continue;
                }
                0xAD => {
                    let low = self.mem[self.pc as usize];
                    self.pc += 1;

                    let hi = self.mem[self.pc as usize];
                    self.pc += 1;

                    self.a = self.mem[u16::from_le_bytes([low, hi]) as usize];

                    ticks -= 4;

                    continue;
                }
                _ => unreachable!(),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::io::Write;

    use super::*;

    const START_ADDR: u16 = 0x0000;

    struct TestMemory([u8; std::u16::MAX as usize]);

    impl TestMemory {
        fn new() -> Self {
            TestMemory([0x00; std::u16::MAX as usize])
        }

        fn write_bytes(&mut self, start: u16, bytes: &[u8]) {
            let _ = (&mut self.0[start as usize..]).write(bytes);
        }
    }

    #[test]
    fn lda_imm() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[
            /* LDA #$01  */ 0xA9, 0x01
        ]);

        let mut cpu = Cpu::new(&mut mem.0);

        cpu.tick(2);

        assert_eq!(cpu.a, 0x01, "A register should contain 0x01");
    }

    #[test]
    fn lda_abs() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[
            /* LDA $CAFE */ 0xAD, 0xFE, 0xCA,
        ]);

        mem.write_bytes(0xCAFE, &[0xFF]);

        let mut cpu = Cpu::new(&mut mem.0);

        cpu.tick(4);

        assert_eq!(cpu.a, 0xFF, "A register should contain 0xFF");
    }

    #[test]
    fn lda_abs_x() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[
            /* LDA $0210,X */ 0xBD, 0x10, 0x02,
        ]);

        mem.write_bytes(0x0210 + 0x12, &[0xAD]);

        let mut cpu = Cpu::new(&mut mem.0);
        cpu.x = 0x12;

        cpu.tick(4);

        assert_eq!(cpu.a, 0xAD, "A register should contain 0xAD");
    }

    #[test]
    fn lda_abs_x_page_cross() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[
            /* LDA $21FF,X */ 0xBD, 0xFF, 0x21,
        ]);

        mem.write_bytes(0x21FF + 0x01, &[0xAD]); // <-- page cross

        let mut cpu = Cpu::new(&mut mem.0);
        cpu.x = 0x01;

        cpu.tick(5);

        assert_eq!(cpu.a, 0xAD, "A register should contain 0xAD");
    }

    #[test]
    fn lda_zpg() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[
            /* LDA $ED */ 0xA5, 0xED,
        ]);

        mem.write_bytes(0x00ED, &[0xFE]);

        let mut cpu = Cpu::new(&mut mem.0);

        cpu.tick(3);

        assert_eq!(cpu.a, 0xFE, "A register should contain 0xFE");
    }

    #[test]
    fn lda_zpg_x() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[
            /* LDA $ED,X */ 0xB5, 0xED,
        ]);

        mem.write_bytes(0x00ED + 0x0011, &[0xCE]);

        let mut cpu = Cpu::new(&mut mem.0);
        cpu.x = 0x11;

        cpu.tick(4);

        assert_eq!(cpu.a, 0xCE, "A register should contain 0xCE");
    }

    #[test]
    fn sta_abs() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[
            /* STA $0200 */ 0x8D, 0x00, 0x02,
        ]);

        let mut cpu = Cpu::new(&mut mem.0);
        cpu.a = 0x01;

        cpu.tick(4);

        assert_eq!(mem.0[0x0200], 0x01, "0x01 should be at addr 0x0200");
    }
}
