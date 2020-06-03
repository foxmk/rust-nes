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

    pub fn tick(&mut self) {
        let byte = self.mem[self.pc as usize];
        self.pc += 1;

        match byte {
            0xA9 => {
                self.a = self.mem[self.pc as usize];
                self.pc += 1;
                return;
            }
            0xAD => {
                let hi = self.mem[self.pc as usize];
                self.pc += 1;

                let low = self.mem[self.pc as usize];
                self.pc += 1;

                self.a = self.mem[u16::from_le_bytes([hi, low]) as usize];
                return;
            }
            0x8D => {
                let hi = self.mem[self.pc as usize];
                self.pc += 1;

                let low = self.mem[self.pc as usize];
                self.pc += 1;

                self.mem[u16::from_le_bytes([hi, low]) as usize] = self.a;
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

    #[test]
    fn load_imm() {
        let mut mem = [0x00; std::u16::MAX as usize];

        let _ = (&mut mem[0x0000..]).write(&[
            /* LDA #$01  */ 0xA9, 0x01
        ]);

        let mut cpu = Cpu::new(&mut mem);

        cpu.tick();

        assert_eq!(cpu.a, 0x01, "A register should contain 0x01");
    }

    #[test]
    fn store_abs() {
        let mut mem = [0x00; std::u16::MAX as usize];

        let _ = (&mut mem[0x0000..]).write(&[
            /* LDA #$01  */ 0xA9, 0x01,
            /* STA $0200 */ 0x8D, 0x00, 0x02,
        ]);

        let mut cpu = Cpu::new(&mut mem);

        for _ in 0..2 {
            cpu.tick();
        }

        assert_eq!(mem[0x0200], 0x01, "0x01 should be at addr 0x0200");
    }

    #[test]
    fn load_ind() {
        let mut mem = [0x00; std::u16::MAX as usize];

        let _ = (&mut mem[0xCAFE..]).write(&[0xFF]);

        let _ = (&mut mem[0x0000..]).write(&[
            /* LDA $CAFE */ 0xAD, 0xFE, 0xCA,
        ]);

        let mut cpu = Cpu::new(&mut mem);

        cpu.tick();

        assert_eq!(cpu.a, 0xFF, "A register should contain 0xFF");
    }
}
