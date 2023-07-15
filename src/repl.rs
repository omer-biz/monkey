use std::io::{self, BufRead, BufReader, Read, Write};

use crate::lexer::Lexer;

const PROMPT: &str = ">> ";

pub struct Repl;

impl Repl {
    pub fn start<R, W>(reader: R, mut _writer: W) -> io::Result<()>
    where
        R: Read,
        W: Write,
    {
        let mut buf_read = BufReader::new(reader);
        let mut line = String::new();

        // write!(writer, "{PROMPT}").unwrap();
        print!("{PROMPT}");
        let _ = io::stdout().flush();

        while buf_read.read_line(&mut line)? > 0 {
            let mut lexer = Lexer::new(&line);

            while let Some(token) = lexer.next_token() {
                // write!(writer, "token: {:?}", token).unwrap();
                println!("token: {:?}", token);
            }

            print!("{PROMPT}");
            let _ = io::stdout().flush();

            line = String::new();
        }

        Ok(())
    }
}
