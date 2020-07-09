use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::fmt;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::io::Write;
use std::ops::{Index, IndexMut};
use std::rc::Rc;

use crate::cpu::{Addr, Cpu, Flag};

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