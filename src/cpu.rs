struct Cpu<'a> {
    mem: &'a mut [u8],
    a: u8,
    pc: u16,
}

impl<'a> Cpu<'a> {
    pub fn new(mem: &'a mut [u8]) -> Self {
        Self {
            mem,
            a: 0x00,
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
                    return;
                }
                0xAD => {
                    let hi = self.mem[self.pc as usize];
                    self.pc += 1;

                    let low = self.mem[self.pc as usize];
                    self.pc += 1;

                    self.a = self.mem[u16::from_le_bytes([hi, low]) as usize];

                    ticks -= 3;

                    return;
                }
                0x8D => {
                    let hi = self.mem[self.pc as usize];
                    self.pc += 1;

                    let low = self.mem[self.pc as usize];
                    self.pc += 1;

                    self.mem[u16::from_le_bytes([hi, low]) as usize] = self.a;

                    ticks -= 4;

                    return;
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

    struct TestMemory([u8; std::u16::MAX as usize]);

    impl TestMemory {
        fn new() -> Self {
            TestMemory([0x00; std::u16::MAX as usize])
        }

        fn write_bytes(&mut self, start: u16, bytes: &[u8]) {
            (&mut self.0[start as usize..]).write(bytes);
        }
    }

    #[test]
    fn lda_imm() {
        let mut mem = TestMemory::new();
        mem.write_bytes(0x0000, &[
            /* LDA #$01  */ 0xA9, 0x01
        ]);

        let mut cpu = Cpu::new(&mut mem.0);

        cpu.tick(2);

        assert_eq!(cpu.a, 0x01, "A register should contain 0x01");
    }

    #[test]
    fn sta_abs() {
        let mut mem = TestMemory::new();
        mem.write_bytes(0x0000, &[
            /* STA $0200 */ 0x8D, 0x00, 0x02,
        ]);

        let mut cpu = Cpu::new(&mut mem.0);
        cpu.a = 0x01;

        cpu.tick(4);

        assert_eq!(mem.0[0x0200], 0x01, "0x01 should be at addr 0x0200");
    }

    #[test]
    fn lda_abs() {
        let mut mem = TestMemory::new();
        mem.write_bytes(0x0000, &[
            /* LDA $CAFE */ 0xAD, 0xFE, 0xCA,
        ]);

        mem.write_bytes(0xCAFE, &[0xFF]);

        let mut cpu = Cpu::new(&mut mem.0);

        cpu.tick(4);

        assert_eq!(cpu.a, 0xFF, "A register should contain 0xFF");
    }
}
