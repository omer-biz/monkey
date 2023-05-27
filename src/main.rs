use std::io;

use monkey_rs::repl::Repl;

fn main() {
    println!(
        "Hello {}! This is the Monkey Programming language!\n",
        std::env::var("USER").unwrap_or("user".to_string())
    );

    println!("Feel free to type in commands\n");

    let repl = Repl::start(io::stdin(), io::stdout());
}
