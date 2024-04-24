mod parser;
mod lexer;

use parser::*;
use super::graphics;

pub struct Interpreter {
    ast : Ast
}

impl Interpreter {

    pub fn new() -> Interpreter {
        Interpreter {ast : Ast::new()}
    }

    pub fn interpret(&mut self, program : &String) -> Result<(), String> {
        let parser = Parser::new(program);
        self.ast = parser.build_ast()?;
        Ok(())
    }
}
