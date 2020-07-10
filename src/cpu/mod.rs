use std::cell::RefCell;
use std::ops::IndexMut;
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
            pc: 0xC000,
            flags: 0b00100100,
            total_cycles: 7,
        }
    }

    pub fn step(&mut self) {
        use Cmd::*;
        use AddrMode::*;

        let byte = self.fetch();

        // Decode
        let (op, addr_mode) = (match &byte {
            0x4C => Some((JMP, Abs)),
            0x86 => Some((STX, Zpg)),
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
        }).unwrap_or_else(|| unimplemented!("Unimplemeted opcode 0x{:02X?}", byte));

        let (operand, eff_addr) = match addr_mode {
            Imp => (0xFF, None),
            Acc => (self.a, None),
            Imm => (self.fetch(), None),
            Zpg => {
                let zpg_addr = self.fetch();
                let eff_addr = u16::from_le_bytes([zpg_addr, 0x00]);
                (self.mem_read(eff_addr), Some(eff_addr))
            }
            ZpgX => {
                let zpg_addr = self.fetch();
                self.inc();
                let eff_addr = u16::from_le_bytes([zpg_addr.wrapping_add(self.x), 0x00]);
                (self.mem_read(eff_addr), Some(eff_addr))
            }
            ZpgY => {
                let zpg_addr = self.fetch();
                self.inc();
                let eff_addr = u16::from_le_bytes([zpg_addr.wrapping_add(self.y), 0x00]);
                (self.mem_read(eff_addr), Some(eff_addr))
            }
            Abs => {
                let addr_lo = self.fetch();
                let addr_hi = self.fetch();
                let eff_addr = u16::from_le_bytes([addr_lo, addr_hi]);
                (self.mem_read(eff_addr), Some(eff_addr))
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

                let eff_addr = u16::from_le_bytes([eff_lo, addr_hi]);
                (self.mem_read(eff_addr), Some(eff_addr))
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

                let eff_addr = u16::from_le_bytes([eff_lo, addr_hi]);
                (self.mem_read(eff_addr), Some(eff_addr))
            }
            Ind => {
                let addr_lo = self.fetch();
                let addr_hi = self.fetch();

                let eff_lo = self.mem_read(u16::from_le_bytes([addr_lo, addr_hi]));
                let eff_hi = self.mem_read(u16::from_le_bytes([addr_lo, addr_hi]).wrapping_add(1));
                let eff_addr = u16::from_le_bytes([eff_lo, eff_hi]);
                (self.mem_read(eff_addr), Some(eff_addr))
            }
            XInd => {
                let zpg_addr = self.fetch().wrapping_add(self.x);
                self.inc();

                let eff_lo = self.mem_read(u16::from_le_bytes([zpg_addr, 0x00]));
                let eff_hi = self.mem_read(u16::from_le_bytes([zpg_addr, 0x00]).wrapping_add(1));

                let eff_addr = u16::from_le_bytes([eff_lo, eff_hi]);
                (self.mem_read(eff_addr), Some(eff_addr))
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

                let eff_addr = u16::from_le_bytes([eff_lo, eff_hi]);
                (self.mem_read(eff_addr), Some(eff_addr))
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
            JMP => {
                self.pc = eff_addr.unwrap();
            }
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
            STX => {
                self.mem.borrow_mut()[eff_addr.unwrap()] = self.x;
            }
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
        if (i as i8) > 0 {
            self.flags ^= Flag::N as u8;
        } else {
            self.flags |= Flag::N as u8;
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
mod test {
    use std::cell::RefCell;
    use std::fmt::{Debug, Display};
    use std::fmt;
    use std::fs::File;
    use std::io::{BufRead, BufReader};
    use std::io::Write;
    use std::ops::{Index, IndexMut};
    use std::rc::Rc;

    use crate::cpu::{Addr, Cpu, Flag};

    use super::*;

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

    #[derive(Debug, Copy, Clone)]
    struct Status(u8);

    impl Display for Status {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let n = if self.0 & 0b10000000 > 0 { "N" } else { "n" };
            let v = if self.0 & 0b01000000 > 0 { "V" } else { "v" };
            let d = if self.0 & 0b00001000 > 0 { "D" } else { "d" };
            let i = if self.0 & 0b00000100 > 0 { "I" } else { "i" };
            let z = if self.0 & 0b00000010 > 0 { "Z" } else { "z" };
            let c = if self.0 & 0b00000001 > 0 { "C" } else { "c" };
            f.write_str(format!("{}{}{}{}{}{}", n, v, d, i, z, c).as_str())
        }
    }

    #[derive(Debug, Clone)]
    struct ReferenceState {
        pc: Addr,
        a: u8,
        x: u8,
        y: u8,
        p: Status,
        cyc: usize,
        op: String,
    }

    #[test]
    fn nestest() {
        let mut mem = ArrayMemory([0x00; u16::MAX as usize]);

        load_test_rom(&mut mem.0);

        let mut cpu = Cpu::new(Rc::new(RefCell::new(mem)));

        let mut op = "<INIT>".to_string();
        let mut prev = 0;
        let mut prev_cpu = 0;

        let log_file = File::open("src/cpu/nestest.log.txt").expect("Log file not found");
        let log_file = BufReader::new(log_file);

        for (step, line) in log_file.lines().enumerate() {
            let want = parse_line(&line.expect("Error reading log file"));

            assert_eq!(cpu.pc, want.pc, "On step {}, after executing {}, PC should be 0x{:04X?}, but was 0x{:04X?}", step, op, want.pc, cpu.pc);
            assert_eq!(cpu.a, want.a, "On step {}, after executing {}, A should be 0x{:02X?}, but was 0x{:02X?}", step, op, want.a, cpu.a);
            assert_eq!(cpu.x, want.x, "On step {}, after executing {}, X should be 0x{:02X?}, but was 0x{:02X?}", step, op, want.x, cpu.x);
            assert_eq!(cpu.y, want.y, "On step {}, after executing {}, Y should be 0x{:02X?}, but was 0x{:02X?}", step, op, want.y, cpu.y);
            // assert_eq!(cpu.flags, state.p.0, "On step {}, after executing {}, P should be {}, but was {}", step, op, state.p, Status(cpu.flags));
            // assert_eq!(cpu.total_cycles, state.cyc, "On step {}, {} should take {} cycles, but took {}", step, op, state.cyc - prev, cpu.total_cycles - prev_cpu);

            op = want.op;
            prev = want.cyc;
            prev_cpu = cpu.total_cycles;
            cpu.step()
        }
    }

    fn load_test_rom(mem: &mut [u8; u16::MAX as usize]) {
        let rom_size = 0x4000;
        let rom = &include_bytes!("nestest.nes")[0x0010..(0x0010 + rom_size)];
        (&mut mem[0xC000..]).write(&rom);
        (&mut mem[0x8000..]).write(&rom);
    }

    fn parse_line(l: &str) -> ReferenceState {
        let pc = u16::from_str_radix(l.get(0..4).unwrap(), 16).expect("Not a hex");
        let a_as_s = l.get(50..52).unwrap();
        let a = u8::from_str_radix(a_as_s, 8).expect("Not a hex");
        let x_as_s = l.get(55..57).unwrap();
        let x = u8::from_str_radix(x_as_s, 16).expect("Not a hex");
        let y_as_s = l.get(60..62).unwrap();
        let y = u8::from_str_radix(y_as_s, 16).expect("Not a hex");
        let p_as_s = l.get(65..67).unwrap();
        let p = u8::from_str_radix(p_as_s, 16).expect("Not a hex");
        let cyc_as_s = l.get(90..).unwrap().trim_end();
        let cyc = usize::from_str_radix(cyc_as_s, 10).expect("Not a dec");
        let op = l.get(16..48).unwrap().trim_end().to_string();

        ReferenceState {
            pc,
            a,
            x,
            y,
            p: Status(p),
            cyc,
            op,
        }
    }
}
