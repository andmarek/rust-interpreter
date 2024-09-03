use clap::Parser;
use lexer::Lexer;
use std::process;

mod ast;
mod lexer;
mod repl;
mod token;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short = 'f', long = "file")]
    filename: Option<String>,
}

fn main() {
    let args = Args::parse();
    if let Some(filename) = args.filename {
        println!("filename is {}", filename.to_string());
        let contents = match std::fs::read_to_string(&filename) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("Error reading file: {}", e);
                process::exit(1);
            }
        };
        let l = Lexer::new(contents);
        for token in l {
            println!("{:?}", token);
        }
    } else {
        println!("No filename provided");
        _ = repl::repl();
    }
}
