use crate::interp::lexer;

pub struct Parser {

}

impl Parser {
    /* should return AST */
    pub fn new() -> Parser {
        Parser{}
    }
    pub fn build_ast(self, string : String) -> () {
        let mut lexer = lexer::Lexer::new(string);
        let token : lexer::Token;
        loop {
            match lexer.get_token() {
                Some(token) => {},
                None => break
            }
        }
    }
}

