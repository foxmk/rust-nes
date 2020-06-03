enum Flag {
    N = 0b10000000,
    V = 0b01000000,
    D = 0b00001000,
    I = 0b00000100,
    Z = 0b00000010,
    C = 0b00000001,
}

enum Register {
    A,
    X,
    Y,
    PC,
    SP,
}

struct Cpu<'a> {
    mem: &'a mut [u8],
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    flags: u8,
}

impl<'a> Cpu<'a> {
    pub fn new(mem: &'a mut [u8]) -> Self {
        Self {
            mem,
            a: 0x00,
            x: 0x00,
            y: 0x00,
            pc: 0x0000,
            flags: 0b00000000,
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

                    if self.a == 0x00 {
                        self.flags |= Flag::Z as u8;
                    }

                    if self.a & 0b10000000 > 0 {
                        self.flags |= Flag::N as u8;
                    }

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

    #[cfg(test)]
    fn test_flag(&self, flag: Flag) -> bool {
        (self.flags & flag as u8) > 0
    }

    #[cfg(test)]
    fn get_byte_register(&self, reg: Register) -> u8 {
        match reg {
            Register::A => self.a,
            Register::X => self.x,
            Register::Y => self.y,
            Register::PC => unreachable!("PC is not a byte register"),
            Register::SP => unreachable!("SP is not a byte register"),
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
        mem.write_bytes(START_ADDR, &[0xA9, 0x01]); // LDA #$01

        let mut cpu = Cpu::new(&mut mem.0);

        cpu.tick(2);

        assert_eq!(cpu.get_byte_register(Register::A), 0x01, "A register should contain 0x01");
    }

    #[test]
    fn lda_sets_negative_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[0xA9, 0b10000001]); // LDA #$81

        let mut cpu = Cpu::new(&mut mem.0);

        cpu.tick(2);

        assert_eq!(cpu.test_flag(Flag::N), true, "N flag should be set")
    }

    #[test]
    fn lda_sets_zero_flag() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[0xA9, 0x00]); // LDA #$00

        let mut cpu = Cpu::new(&mut mem.0);

        cpu.tick(2);

        assert_eq!(cpu.test_flag(Flag::Z), true, "Z flag should be set")
    }

    #[test]
    fn lda_abs() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[0xAD, 0xFE, 0xCA]); // LDA $CAFE
        mem.write_bytes(0xCAFE, &[0xFF]);

        let mut cpu = Cpu::new(&mut mem.0);

        cpu.tick(4);

        assert_eq!(cpu.get_byte_register(Register::A), 0xFF, "A register should contain 0xFF");
    }

    #[test]
    fn lda_abs_x() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[0xBD, 0x10, 0x02]); // LDA $0210,X
        mem.write_bytes(0x0210 + 0x12, &[0xAD]);

        let mut cpu = Cpu::new(&mut mem.0);
        cpu.x = 0x12;

        cpu.tick(4);

        assert_eq!(cpu.get_byte_register(Register::A), 0xAD, "A register should contain 0xAD");
    }

    #[test]
    fn lda_abs_x_page_cross() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[0xBD, 0xFF, 0x21]); // LDA $21FF,X
        mem.write_bytes(0x21FF + 0x01, &[0xAD]); // <-- page cross

        let mut cpu = Cpu::new(&mut mem.0);
        cpu.x = 0x01;

        cpu.tick(5);

        assert_eq!(cpu.get_byte_register(Register::A), 0xAD, "A register should contain 0xAD");
    }

    #[test]
    fn lda_zpg() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[0xA5, 0xED]); // LDA $ED
        mem.write_bytes(0x00ED, &[0xFE]);

        let mut cpu = Cpu::new(&mut mem.0);

        cpu.tick(3);

        assert_eq!(cpu.get_byte_register(Register::A), 0xFE, "A register should contain 0xFE");
    }

    #[test]
    fn lda_zpg_x() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[0xB5, 0xED]); // LDA $ED,X
        mem.write_bytes(0x00ED + 0x0011, &[0xCE]);

        let mut cpu = Cpu::new(&mut mem.0);
        cpu.x = 0x11;

        cpu.tick(4);

        assert_eq!(cpu.get_byte_register(Register::A), 0xCE, "A register should contain 0xCE");
    }

    #[test]
    fn sta_abs() {
        let mut mem = TestMemory::new();
        mem.write_bytes(START_ADDR, &[0x8D, 0x00, 0x02]); // STA $0200

        let mut cpu = Cpu::new(&mut mem.0);
        cpu.a = 0x01;

        cpu.tick(4);

        assert_eq!(mem.0[0x0200], 0x01, "0x01 should be at addr 0x0200");
    }
}
