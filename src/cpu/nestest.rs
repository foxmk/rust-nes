use std::cell::{RefCell, Ref};
use std::fs::File;
use std::io::{BufReader, BufRead};
use std::ops::{Index, IndexMut};
use std::rc::Rc;

use crate::cpu::{Addr, Cpu, Flag};
use crate::cpu::Flag::*;
use crate::cpu::test_helpers::*;
use crate::cpu::test_helpers::Register::*;
use std::fmt::{Debug, Display, Formatter};
use core::fmt;

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

impl Display for  Status {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let n = if self.0 & 0b10000000 >0 { "N" } else { "n" };
        let v = if self.0 & 0b01000000 >0 { "V" } else { "v" };
        let d = if self.0 & 0b00001000 >0 { "D" } else { "d" };
        let i = if self.0 & 0b00000100 >0 { "I" } else { "i" };
        let z = if self.0 & 0b00000010 >0 { "Z" } else { "z" };
        let c = if self.0 & 0b00000001 >0 { "C" } else { "c" };
        f.write_str(format!("{}{}{}{}{}{}", n, v, d, i, z, c).as_str())
    }
}

#[derive(Debug, Copy, Clone)]
struct ReferenceState {
    pc: Addr,
    a: u8,
    x: u8,
    y: u8,
    p: Status
}

fn parse_line(l: &str) -> ReferenceState{
    let pc = u16::from_str_radix(l.get(0..4).unwrap(), 16).expect("Not a hex");
    let a_as_s = l.get(50..52).unwrap();
    let a = u8::from_str_radix(a_as_s, 8).expect("Not a hex");
    let x_as_s = l.get(55..57).unwrap();
    let x = u8::from_str_radix(x_as_s, 16).expect("Not a hex");
    let y_as_s = l.get(60..62).unwrap();
    let y = u8::from_str_radix(y_as_s, 16).expect("Not a hex");
    let p_as_s = l.get(65..67).unwrap();
    let p = u8::from_str_radix(p_as_s, 16).expect("Not a hex");

    ReferenceState {
        pc,
        a,
        x,
        y,
        p: Status(p)
    }
}

#[test]
fn nestest() {
    let mem = Rc::new(RefCell::new(ArrayMemory([0x00; u16::MAX as usize])));
    // {
    //     mem.borrow_mut().0.write_slice(0x0000, include_bytes!("test_roms/nestest/nestest.nes"));
    // }
    let mut cpu = Cpu::new(mem.clone());
    cpu.pc = 0xC000;
    cpu.flags |= Flag::I as u8;

    let log_file = File::open("src/cpu/nestest.log.txt").unwrap();
    let log_file = BufReader::new(log_file);

    for (step, line) in log_file.lines().enumerate() {
        let l = line.unwrap();
        let state = parse_line(&l);
        println!("{:04X?}", &state);

        assert_eq!(cpu.pc, state.pc, "On step {} PC should be 0x{:04X?}, but was 0x{:04X?}", step, state.pc, cpu.pc);
        assert_eq!(cpu.a, state.a, "On step {} A should be 0x{:02X?}, but was 0x{:02X?}", step, state.a, cpu.a);
        assert_eq!(cpu.x, state.x, "On step {} X should be 0x{:02X?}, but was 0x{:02X?}", step, state.x, cpu.x);
        assert_eq!(cpu.y, state.y, "On step {} Y should be 0x{:02X?}, but was 0x{:02X?}", step, state.y, cpu.y);
        assert_eq!(cpu.flags, state.p.0, "On step {} P should be {}, but was {}", step, state.p, Status(cpu.flags));

        cpu.step()
    }
}