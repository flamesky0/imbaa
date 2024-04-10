
#[derive(Clone)]
pub enum Token {
    ID(String),
    // -- integer numbers
    NUMBER(i64), // regex
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
    // -- flow control
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
    // -- --
    FUNCTION, // function
    LP, // (
    RP, // )
    RETURN, // return
}

/* struct {
    token : Token,
    string : &'static str
} */
static TABLE_OF_TOKENS : [(Token, &'static str); 36] =
                            [
                                (Token::BOX, "BOX"),
                                (Token::DONE, "DONE"),
                                (Token::DO, "DO"),
                                (Token::DROP, "DROP"),
                                (Token::EMPTY, "EMPTY"),
                                (Token::EXIT, "EXIT"),
                                (Token::ELDEF, "ELDEF"),
                                (Token::ELUND, "ELUND"),
                                (Token::FALSE, "FALSE"),
                                (Token::FINISH, "FINISH"),
                                (Token::FUNCTION, "FUNCTION"),
                                (Token::INF, "INF"),
                                (Token::NINF, "-INF"),
                                (Token::INF, "INF"),
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
                                (Token::RP, ")"),
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
        while self.index < self.source.len() {
            unsafe {
                while (*ptr).is_ascii_whitespace() {
                    self.index += 1;
                    continue
                }
                for (token, string) in TABLE_OF_TOKENS.iter() {
                    if Lexer::cmp(ptr.add(self.index), string) {
                        self.index += string.len();
                        return Some((*token).clone())
                    }
                }
            }
        }
        None
    }

    fn cmp(ptr : *const u8, substr : &str) -> bool {
        let mut i = 0;
        while i < substr.len() {
            unsafe {
                if *ptr.add(i) != substr.as_bytes()[i] {
                    return false;
                }
            }
            i = i + 1;
        }
        return true;
    }
}

