
#[derive(Clone,Debug,PartialEq)]
pub enum Token {
    ID(String),
    // -- integer numbers
    NUM(i64), // only decimal numbers :))
    INF, // inf
    NINF, // -inf
    NAN, // nan
    // -- logic
    TRUE, //  true
    FALSE, //  false
    UNDEF, // undef
    // -- cell states
    EMPTY, // empty
    WALL, // wall
    BOX, // box
    EXIT, // exit
    //UNDEF, -- the same for cell and logic
    VAR, // var, variable of any type
    INT,
    BOOL,
    CELL,
    // -- flow control
    IF,
    WHILE, // while
    DO, // do
    DONE, // done
    FINISH, // finish
    ELDEF, // eldef
    ELUND, // elund
    // -- arithmetics
    PLUS, // +
    MINUS, // -
    XOR, // ^
    HASH, // #
    ASSIGN, // :=
    EQUALS, // =
    LESS, // <
    MORE, // >
    // -- robot specific command
    LEFT, // left
    RIGHT, // right
    LOAD, // load
    DROP, // drop
    LOOK, // look
    TEST, // test
    FORWARD, // forward
    BACKWARD, // backward
    // -- --
    FUNCTION, // function
    LP, // (
    LB, // [
    RP, // )
    RB, // ]
    SEMICOLON, // ;
    COMMA, // ,
    RETURN, // return
    UNKNOWN, // return
    EOF
}

/* table of static tokens */
static TABLE_OF_TOKENS : [(Token, &'static str); 45] =
                            [
                                (Token::BACKWARD, "BACKWARD"),
                                (Token::BOOL, "BOOL"),
                                (Token::BOX, "BOX"),
                                (Token::CELL, "CELL"),
                                (Token::DONE, "DONE"),
                                (Token::DO, "DO"),
                                (Token::DROP, "DROP"),
                                (Token::EMPTY, "EMPTY"),
                                (Token::EXIT, "EXIT"),
                                (Token::ELDEF, "ELDEF"),
                                (Token::ELUND, "ELUND"),
                                (Token::FALSE, "FALSE"),
                                (Token::FINISH, "FINISH"),
                                (Token::FORWARD, "FORWARD"),
                                (Token::FUNCTION, "FUNCTION"),
                                (Token::IF, "IF"),
                                (Token::INF, "INF"),
                                (Token::INT, "INT"),
                                (Token::NINF, "-INF"),
                                (Token::LEFT, "LEFT"),
                                (Token::LOAD, "LOAD"),
                                (Token::LOOK, "LOOK"),
                                (Token::NAN, "NAN"),
                                (Token::RETURN, "RETURN"),
                                (Token::RIGHT, "RIGHT"),
                                (Token::TEST, "TEST"),
                                (Token::TRUE, "TRUE"),
                                (Token::UNDEF, "UNDEF"),
                                (Token::VAR, "VAR"),
                                (Token::WALL, "WALL"),
                                (Token::WHILE, "WHILE"),
                                (Token::PLUS, "+"),
                                (Token::MINUS, "-"),
                                (Token::XOR, "^"),
                                (Token::HASH, "#"),
                                (Token::EQUALS, "="),
                                (Token::LESS, "<"),
                                (Token::MORE, ">"),
                                (Token::ASSIGN, ":="),
                                (Token::LP, "("),
                                (Token::LB, "["),
                                (Token::RP, ")"),
                                (Token::RB, "]"),
                                (Token::SEMICOLON, ";"),
                                (Token::COMMA, ",")
                            ];
pub struct Lexer {
    source : String,
    index : usize,
}

impl Lexer {
    /* interpreter is case insensitive, so we do make uppercase */
    pub fn new(source : String) -> Lexer {
        Lexer {source : source.to_uppercase(), index : 0}
    }

    pub fn get_token(&mut self) -> Option<Token> {
        let ptr = self.source.as_ptr();
        while self.index < self.source.len() - 1 {
            unsafe {
                while (*ptr.add(self.index)).is_ascii_whitespace() {
                    self.index += 1;
                }
                /* match static tokens */
                for (token, string) in TABLE_OF_TOKENS.iter() {
                    //println!("{:?} {}", token, string);
                    if self.cmp(ptr.add(self.index), string) {
                        self.index += string.len();
                        return Some((*token).clone())
                    }
                }
                match self.match_id(ptr.add(self.index)) {
                    Some(string) => return Some(Token::ID(string)),
                    None => ()
                }
                match self.match_num(ptr.add(self.index)) {
                    Some(num) => return Some(Token::NUM(num)),
                    None => ()
                }
                println!("lexer: unknown symbol {:x} at {}", ptr.add(self.index) as u8, self.index);
                /* nothing mathed, lexic error */
                return Some(Token::UNKNOWN);
            }
        }
        None
    }

    unsafe fn match_id(&mut self, ptr : *const u8) -> Option<String> {
        let mut string = String::with_capacity(64);
        let mut i = 0;
        while ptr.add(i) < self.source.as_ptr().add(self.source.len()) &&
                (*ptr.add(i)).is_ascii_alphabetic() {
                string.push(*ptr.add(i) as char);
                i = i + 1;
        }
        if !string.is_empty() {
            self.index += i;
            return Some(string);
        }
        return None;
    }

    unsafe fn match_num(&mut self, ptr : *const u8) -> Option<i64> {
        let mut string = String::with_capacity(64);
        let mut i = 0;
        while ptr.add(i) < self.source.as_ptr().add(self.source.len()) &&
                (*ptr.add(i)).is_ascii_digit() {
                string.push(*ptr.add(i) as char);
                i = i + 1;
        }
        if !string.is_empty() {
            self.index += i;
            return Some(string.parse::<i64>().unwrap());
        }
        return None;
    }

    unsafe fn cmp(&self, ptr : *const u8, substr : &str) -> bool {
        let mut i = 0;
        while i < substr.len() {
                if ptr.add(i) >= self.source.as_ptr().add(self.source.len()) ||
                    *ptr.add(i) != substr.as_bytes()[i] {
                    return false;
                }
            i = i + 1;
        }
        return true;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test1() {
        let _string1 = String::from("   VAR INT BOOL CELL ; + - < > = while do finIsh donw forward ; look ) (  ) () 381
                                    8 + 34 := 3 ^ drop reTURn elund eldef = < ; :=  xor bool nan inf -INF --InF");

        let _string2 = String::from("var misha, masha, jopa; bool mesha;");
        let mut lexer = Lexer::new(_string2);
        while let Some(token) = lexer.get_token() {
            println!("{:?}", token);
            match token {
                Token::UNKNOWN => return,
                _ => ()
            }
        }
    }
}
