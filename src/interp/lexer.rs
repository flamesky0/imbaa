
#[derive(Clone,Debug,PartialEq)]
pub enum Token {
    ID(String),
    // -- integer numbers
    NUM(i64), // only decimal numbers :))
    INF, // inf
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
static TABLE_OF_KEYWORDS : [(Token, &'static str); 44] =
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
pub struct Lexer <'a> {
    index : std::iter::Peekable<std::str::Chars<'a>>
}

impl <'a> Lexer<'a> {
    /* interpreter is case insensitive, so we do make uppercase */
    pub fn new(source : &'a String) -> Lexer<'a> {
        Lexer {index : source.chars().peekable()}
    }

    fn skip_whitespaces(&mut self) {
        while let Some(letter) = self.index.peek() {
            if letter.is_ascii_whitespace() {
                self.index.next();
            } else {
                break
            }
        }
    }

    fn token_is_punctuation(token : &Token) -> bool {
        use Token::*;
        match token {
            PLUS | MINUS | XOR | HASH | EQUALS |
                LESS | MORE | ASSIGN | LP | LB |
                RP | RB | SEMICOLON | COMMA => {
                true
            }
            _ => {
                false
            }
        }
    }

    fn match_keyword(&mut self) -> Option<Token> {
        let save = self.index.clone();
        'outer: for (token, string) in &TABLE_OF_KEYWORDS {
            self.index = save.clone();
            for letter in string.chars() {
                if let Some(lett) = self.index.peek() {
                    if lett.to_ascii_uppercase() != letter {
                        continue 'outer;
                    }
                    //println!("letter: {}", lett);
                } else {
                    continue 'outer;
                }
                self.index.next();
            }
            if Lexer::token_is_punctuation(token) {
                return Some(token.clone());
            } else {
                /* dont watch here */
                if let Some(l) = self.index.peek() {
                    if l.is_ascii_whitespace() ||
                        l.is_ascii_punctuation() {
                            return Some(token.clone());
                    }
                    self.index = save;
                    return None;
                } else {
                    return Some(token.clone());
                }
            }
        }
        None
    }

    fn match_id(&mut self) -> Option<String> {
        let mut string = String::new();
        let save = self.index.clone();
        let mut prev = self.index.clone();
        if let Some(letter) = self.index.peek() {
            if letter.is_ascii_alphabetic() {
                //string.push(*letter);
                //self.index.next();
                while let Some(letter) = self.index.next() {
                    if letter.is_ascii_alphanumeric() {
                        prev = self.index.clone();
                        string.push(letter);
                    } else {
                        self.index = prev;
                        return Some(string);
                    }
                }
            } else {
                self.index = save;
                return None;
            }
        }
        None
    }

    fn match_num(&mut self) -> Option<i64> {
        let mut string = String::new();
        let mut matched : bool = false;

        while let Some(letter) = self.index.peek() {
            if letter.is_ascii_digit() {
                string.push(*letter);
                self.index.next();
                matched = true;
            } else {
                break;
            }
        }

        if matched {
            return Some(string.parse::<i64>().unwrap());
        }
        None
    }
}

impl<'a> Iterator for Lexer <'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        //println!("get token called");
        // FOR DEBUG
        /* let mut buffer = String::new();
        let stdin = std::io::stdin();
        stdin.read_line(&mut buffer).unwrap(); */

        self.skip_whitespaces();
        //println!("Whitespaces gone");
        if let Some(keyword) = self.match_keyword() {
            return Some(keyword);
        }
        if let Some(id) = self.match_id() {
            return Some(Token::ID(id));
        }
        if let Some(num) = self.match_num() {
            return Some(Token::NUM(num));
        }
        if let Some(_) = self.index.next() {
            /* Nothing matched, but input stream is not empty*/
            return Some(Token::UNKNOWN);
        }
        None
    }
}

impl Default for &Token {
    fn default() -> Self {
        return &Token::EOF;
    }
}

impl Default for Token {
    fn default() -> Self {
        return Token::EOF;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn lexer() {
        let _string1 = String::from("   VAR 9,  883;INT BOOL CELL ; + - < > = while don, +don0; don1 don2 don10 misha do finIsh done forward ; look ) (  ) () 381
                                    8 + 34 := 3; ^ drop reTURn elund 8  ;:=,   ieldef = < ; :=  xor bool nan inf -INF --InF");

        let _string2 = String::from("var , ; bool  ;;,:=;var;bool;do([ empty  ] drop[  ])");
        let _string3 = String::from("if 5 > 2; a do look; done ");
        let mut buffer = String::new();
        let stdin = std::io::stdin(); // We get `Stdin` here.
        let lexer = Lexer::new(&_string1);

        for token in lexer.into_iter() {
            println!("{:?}", token);
            match token {
                Token::UNKNOWN => return,
                _ => ()
            }
            stdin.read_line(&mut buffer).unwrap();
        }
    }
}
