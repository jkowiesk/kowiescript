// simple io handler for kowiesscript

use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader, Cursor, Read, Result};
use std::iter::Peekable;
pub enum Input {
    File(String),
    String(String),
}

// all whitespace characters in utf-8 encoding
pub const WHITESPACES: [u8; 7] = [32, 9, 10, 11, 12, 13, 160];

pub struct ChrIterator<'a> {
    buf: Box<dyn BufRead + 'a>,
    pub curr: Option<String>,
}

impl<'a> ChrIterator<'a> {
    pub fn new(input: Input) -> Result<Peekable<ChrIterator<'a>>> {
        let buf = read_input(input)?;
        Ok(ChrIterator { buf, curr: None }.peekable())
    }
}

impl Iterator for ChrIterator<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buf = [0; 4];
        match self.buf.read(&mut buf[..1]) {
            Ok(0) => None,
            Ok(_) => {
                let first_byte = buf[0];
                let width = if first_byte & 0x80 == 0x00 {
                    1
                } else if first_byte & 0xE0 == 0xC0 {
                    2
                } else if first_byte & 0xF0 == 0xE0 {
                    3
                } else if first_byte & 0xF8 == 0xF0 {
                    4
                } else {
                    // invalid utf-8 sequence
                    return None;
                };

                match self.buf.read(&mut buf[1..width]) {
                    Ok(n) if n == width - 1 => {
                        let s = std::str::from_utf8(&buf[..width]).ok()?;
                        let c = s.chars().next()?;
                        self.curr = Some(c.clone().to_string());
                        Some(c)
                    }
                    _ => None,
                }
            }
            Err(_) => None,
        }
    }
}

pub fn read_input(input: Input) -> Result<Box<dyn BufRead>> {
    match input {
        Input::File(path) => {
            let file = File::open(path)?;
            let buf = BufReader::new(file);
            Ok(Box::new(buf))
        }
        Input::String(s) => Ok(Box::new(Cursor::new(s))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_input() {
        let string = String::from("ąę");
        let mut chr_iter = ChrIterator::new(Input::String(string)).unwrap();

        assert_eq!(chr_iter.next(), Some('ą'));
        assert_eq!(chr_iter.next(), Some('ę'));
    }

    #[test]
    fn test_icons_input() {
        let string = String::from("🔥⭐🌂");
        let mut chr_iter = ChrIterator::new(Input::String(string)).unwrap();

        assert_eq!(chr_iter.next(), Some('🔥'));
        assert_eq!(chr_iter.next(), Some('⭐'));
        assert_eq!(chr_iter.next(), Some('🌂'));
    }

    #[test]
    fn test_file_input() {
        let mut chr_iter =
            ChrIterator::new(Input::File("src/tests/data/test_file.ks".to_string())).unwrap();

        assert_eq!(chr_iter.next(), Some('ą'));
        assert_eq!(chr_iter.next(), Some('ć'));
        assert_eq!(chr_iter.next(), Some('ż'));
    }
}
