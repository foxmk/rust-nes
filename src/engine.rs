use std::collections::HashMap;
use std::convert::From;
use std::error;
use std::fmt;
use std::fs::File;
use std::time;

use minifb;
use png;

use lazy_static::lazy_static;

const WIDTH: usize = 8 + 256 + 8 + 200;
const HEIGHT: usize = 8 + 240 + 8;
const BLUE: u32 = 0xFF000088;

fn read_font_file() -> HashMap<char, [u32; 64]> {
    let decoder = png::Decoder::new(File::open("font.png").expect("Font file not found"));
    let (info, mut reader) = decoder.read_info().expect("Unable to read font file");
    let mut pixels = vec![0; info.buffer_size()];
    reader.next_frame(&mut pixels).expect("Unable to read font file");

    let mut sprites: HashMap<char, [u32; 64]> = HashMap::new();

    for c in 0..=0xFF as u8 {
        let sprite_x = (c % 16) as u32;
        let sprite_y = (c / 16) as u32;
        let sprite_start_x = sprite_x * 8 + sprite_x * 2 + 1; // Fringe
        let sprite_start_y = sprite_y * 8 + sprite_y * 2 + 1;

        let mut buf = [0; 64];
        for y in 0..8 {
            for x in 0..8 {
                let img_x = sprite_start_x + x;
                let img_y = sprite_start_y + y;

                let r = pixels[((img_x + img_y * info.width) as usize) * 3 + 0];
                let g = pixels[((img_x + img_y * info.width) as usize) * 3 + 1];
                let b = pixels[((img_x + img_y * info.width) as usize) * 3 + 2];

                buf[x as usize + 8 * y as usize] = unsafe { std::mem::transmute::<[u8; 4], u32>([0x00, r, g, b]) }.to_be();
            }
        }

        sprites.insert(c as char, buf);
    }

    sprites
}

lazy_static! {
static ref GLYPHS: HashMap<char, [u32; 64]> = {
    read_font_file()
};
}

pub struct Screen {
    buf: [u32; WIDTH * HEIGHT]
}

impl Screen {
    pub fn new() -> Self {
        Self {
            buf: [BLUE; WIDTH * HEIGHT]
        }
    }

    pub fn draw_pixel(&mut self, color: u32, x: usize, y: usize) {
        let pixel_size = 1;
        for px in 0..pixel_size {
            for py in 0..pixel_size {
                let screen_x = x * pixel_size + px;
                let screen_y = y * pixel_size + py;

                self.buf[screen_x + WIDTH * screen_y] = color;
            }
        }
    }

    pub fn draw_rect(&mut self, w: usize, h: usize, color: u32, x: usize, y: usize) {
        for i in 0..w {
            for j in 0..h {
                self.draw_pixel(color, x + i, y + j);
            }
        }
    }

    pub fn draw_sprite(&mut self, sprite: &[u32; 64], x: usize, y: usize) {
        for (i, p) in sprite.iter().enumerate() {
            let sprite_x = i % 8;
            let sprite_y = i / 8;

            self.draw_pixel(*p, x + sprite_x, y + sprite_y);
        }
    }

    pub fn draw_string(&mut self, x: usize, y: usize, s: &str) {
        for (i, c) in s.chars().enumerate() {
            self.draw_sprite(GLYPHS.get(&(c as char)).unwrap(), x + i * 8, y);
        }
    }
}

pub trait Update {
    fn update(&mut self, screen: &mut Screen, window: &minifb::Window) -> bool;
}

#[derive(Debug)]
pub enum NesEmuError {
    SpriteLoadingError(String),
    Other(String),
}

impl fmt::Display for NesEmuError {
    fn fmt(&self, _: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}

impl error::Error for NesEmuError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            NesEmuError::Other(_) => None,
            NesEmuError::SpriteLoadingError(_) => None,
        }
    }
}

impl From<std::io::Error> for NesEmuError {
    fn from(inner: std::io::Error) -> Self {
        NesEmuError::SpriteLoadingError(inner.to_string())
    }
}

impl From<minifb::Error> for NesEmuError {
    fn from(inner: minifb::Error) -> Self {
        NesEmuError::Other(inner.to_string())
    }
}

pub struct Engine<'a> {
    window: minifb::Window,
    screen: Screen,
    show_dbg: bool,
    game: &'a mut dyn Update,
}

impl<'a> Engine<'a> {
    pub fn build(game: &'a mut dyn Update) -> Result<Self, NesEmuError> {
        let window = minifb::Window::new("NES emulator", WIDTH, HEIGHT, minifb::WindowOptions {
            resize: false,
            ..minifb::WindowOptions::default()
        })?;

        let screen = Screen::new();

        let show_dbg = true;

        Ok(Self {
            window,
            screen,
            show_dbg,
            game,
        })
    }

    pub fn run(&mut self) -> Result<(), NesEmuError> {
        loop {
            let now = time::Instant::now();

            let should_continue = self.game.update(
                &mut self.screen,
                &self.window,
            );

            if !should_continue {
                break;
            }

            self.window.update_with_buffer(&self.screen.buf)?;

            let elapsed = time::Instant::now().duration_since(now);
            let fps = 1000000.0 / elapsed.as_micros() as f32;

            if self.show_dbg {
                self.screen.draw_string(8, 8, format!("FPS: {}", fps).as_str());
            }
        }

        Ok(())
    }
}
