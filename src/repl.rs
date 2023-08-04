use std::io::{self, BufRead, BufReader, Read, Write};

use crate::{lexer::Lexer, parser::Parser};

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

        print!("{PROMPT}");
        let _ = io::stdout().flush();

        while buf_read.read_line(&mut line)? > 0 {
            let lexer = Lexer::new(&line);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            if parser.errors.len() != 0 {
                print_parser_errors(&parser.errors);
                line = String::new();
                print!("{PROMPT}");
                let _ = io::stdout().flush();
                continue;
            }
            println!("{}", program);

            print!("{PROMPT}");
            let _ = io::stdout().flush();

            line = String::new();
        }

        Ok(())
    }
}

fn print_parser_errors(errors: &[String]) {
    for e in errors {
        println!("\t{e}");
    }
}
