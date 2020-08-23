use std::collections::HashMap;
use std::convert::From;
use std::error;
use std::fmt;
use std::fs::File;
use std::time;

use minifb;
use png;

use lazy_static::lazy_static;

mod cpu;

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

#[derive(Debug)]
enum NesEmuError {
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

fn draw_pixel(buffer: &mut [u32], color: u32, x: usize, y: usize) {
    let pixel_size = 1;
    for px in 0..pixel_size {
        for py in 0..pixel_size {
            let screen_x = x * pixel_size + px;
            let screen_y = y * pixel_size + py;

            buffer[screen_x + WIDTH * screen_y] = color;
        }
    }
}

fn draw_rect(buffer: &mut [u32], w: usize, h: usize, color: u32, x: usize, y: usize) {
    for i in 0..w {
        for j in 0..h {
            draw_pixel(buffer, color, x + i, y + j);
        }
    }
}

fn draw_sprite(buffer: &mut [u32], sprite: &[u32; 64], x: usize, y: usize) {
    for (i, p) in sprite.iter().enumerate() {
        let sprite_x = i % 8;
        let sprite_y = i / 8;

        draw_pixel(buffer, *p, x + sprite_x, y + sprite_y);
    }
}

fn draw_string(buffer: &mut [u32], x: usize, y: usize, s: &str) {
    for (i, c) in s.chars().enumerate() {
        draw_sprite(buffer, GLYPHS.get(&(c as char)).unwrap(), x + i * 8, y);
    }
}

fn main() -> Result<(), NesEmuError> {
    let mut window = minifb::Window::new("NES emulator", WIDTH, HEIGHT, minifb::WindowOptions {
        resize: false,
        ..minifb::WindowOptions::default()
    })?;

    let mut buf: [u32; WIDTH * HEIGHT] = [BLUE; WIDTH * HEIGHT];

    let mut running = true;
    let mut fps: f32 = 0.0;
    let mut show_dbg = false;

    while running {
        let now = time::Instant::now();

        if window.is_key_pressed(minifb::Key::Escape, minifb::KeyRepeat::No) {
            running = false;
        }

        if window.is_key_pressed(minifb::Key::F10, minifb::KeyRepeat::No) {
            show_dbg = !show_dbg;
        }


        draw_rect(&mut buf, 256, 240, 0x00000000, 8, 8);

        draw_string(&mut buf, 8 + 8 + 256, 8, "Hello, world!");

        if show_dbg {
            draw_string(&mut buf, 8, 8, format!("FPS: {}", fps).as_str());
        }

        window.update_with_buffer(&buf)?;

        let elapsed = time::Instant::now().duration_since(now);
        fps = 1000000.0 / elapsed.as_micros() as f32;

        window.set_title(format!("NES emulator. FPS: {}", fps).as_str())
    }

    Ok(())
}
