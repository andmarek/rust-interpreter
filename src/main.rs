use lexer::Lexer;

mod lexer;
mod repl;
mod token;

fn main() {
    repl::repl();
}
