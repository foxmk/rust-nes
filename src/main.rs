use minifb::Window;

mod cpu;
mod engine;

struct Emulator {
    show_dbg: bool,
    color: u32,
}

impl Emulator {
    pub fn new() -> Self {
        Self { show_dbg: true, color: 0x000000 }
    }
}

impl engine::Update for Emulator {
    fn update(&mut self, screen: &mut engine::Screen, window: &minifb::Window) -> bool {
        if window.is_key_pressed(minifb::Key::Escape, minifb::KeyRepeat::No) {
            return false;
        }

        if window.is_key_pressed(minifb::Key::F10, minifb::KeyRepeat::No) {
            self.show_dbg = !self.show_dbg;
        }

        self.color = self.color.wrapping_add(1);

        screen.draw_rect(256, 240, self.color, 8, 8);
        screen.draw_string(8 + 8 + 256, 8, "Hello, world!");

        true
    }
}

fn main() -> Result<(), engine::NesEmuError> {
    let mut game = Emulator::new();
    let mut eng = engine::Engine::build(&mut game).unwrap();
    eng.run()
}
