use std::fs::File;

pub enum Token {
    /* list of all tokens */
}

pub struct Lexer {
    source : File,
    lex_begin : u64,
    forward : u64
}

impl Lexer {
    pub fn new(source : File) -> Lexer {
        Lexer {source, lex_begin : 0, forward : 0}
    }
    pub fn get_token() -> Token {
        /* do smth */
        todo!()
    }
}

