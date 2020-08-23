use std::collections::HashMap;
use std::convert::From;
use std::error;
use std::fmt;
use std::fs::File;
use std::time;

use lazy_static::lazy_static;
use minifb;
use png;

mod cpu;
mod engine;

struct Emulator {
    show_dbg: bool,
    color: u32
}

impl engine::Update for Emulator {
    fn update(&mut self) {
        self.color = self.color.wrapping_add(1);
    }
}

impl engine::Draw for Emulator {
    fn draw(&self, screen: &mut engine::Screen) {
        screen.draw_rect(256, 240, self.color, 8, 8);

        screen.draw_string(8 + 8 + 256, 8, "Hello, world!");
    }
}

fn main() -> Result<(), engine::NesEmuError> {
    let mut game = Emulator { show_dbg: true, color: 0x000000 };
    let mut eng = engine::Engine::build(&mut game).unwrap();
    eng.run()
}
