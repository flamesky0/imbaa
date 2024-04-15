use crate::interp::lexer;

#[derive(Clone, Copy, Debug)]
enum BoolVal {
    FALSE = 0,
    TRUE,
    UNDEF
}

#[derive(Clone, Copy, Debug)]
enum CellVal {
    EMPTY,
    WALL,
    BOX,
    EXIT,
    UNDEF
}

#[derive(Clone, Copy, Debug)]
enum Val {
    UNDEF,
    INT(i64),
    BOOL(BoolVal),
    CELL(CellVal)
}

impl Val {
    pub fn new() -> Val {
        Val::UNDEF
    }
}
#[derive(Debug)]
struct Var {
    name : String,
    val : Val
}

impl Var {
    pub fn new() -> Var {
        Var {name : String::new(), val : Val::new()}
    }

    pub fn from(name : String, val : Val) -> Var {
        Var {name, val}
    }
}
#[derive(Debug)]
struct IfStmt {
    cond : Box<Stmt>,
    stmts : Vec<Stmt>,
    eldef : Option<Vec<Stmt>>,
    elund : Option<Vec<Stmt>>
}

#[derive(Debug)]
struct WhileStmt {
    cond : Box<Stmt>,
    stmts : Vec<Stmt>,
    finish : Option<Vec<Stmt>>
}

#[derive(Debug)]
enum OP {
    LESS,
    MORE,
    EQUA,
    PLUS,
    MINU,
    XOR
}

#[derive(Debug)]
enum Term {
    EXPR(Box<LogicExpr>),
    ID(Var),
    NUM(i64)
}

#[derive(Debug)]
struct LogicExpr {
    operation : Option<OP>,
    lterm : Term,
    rterm : Term
}

#[derive(Debug)]
enum Stmt {
    IF(IfStmt),
    WHILE(WhileStmt),
    ASSGN(Var, Box<Stmt>),
    CALL(String, Var),
    LOAD(Val),
    DROP(Val),
    FORWARD(Val),
    BACKWARD(Val),
    LEXPR(LogicExpr),
    RETURN,
    LOOK,
    TEST,
    LEFT,
    RIGHT
}

#[derive(Debug)]
struct Func {
    name : String,
    param : Var,
    vars : Vec<Var>,
    stmts : Vec<Stmt>
}

impl Func {
    pub fn new() ->  Func {
        Func {name : String::new(),
              param : Var::new(),
              vars : Vec::new(), stmts : Vec::new()}
    }
}

#[derive(Debug)]
struct Ast {
    vars : Vec<Var>,
    funcs : Vec<Func>
}

pub struct Parser {
    lexer : lexer::Lexer,
    ast : Ast,
    tokens : Vec<lexer::Token>,
    index : usize
}

use lexer::Lexer;

impl Parser {

    pub fn new(string : String) -> Parser {
        Parser {
                lexer : Lexer::new(string),
                ast : Ast {vars: Vec::new(), funcs : Vec::new()},
                tokens : Vec::with_capacity(1024),
                index : 0
        }
    }

    fn parse_var_decl(&mut self) -> Option<Vec<Var>> {
        use lexer::Token;
        let val : Val;
        let mut vars : Vec<Var> = Vec::new();

        match self.tokens[self.index] {
            Token::VAR => {
                val = Val::UNDEF;
            }
            Token::BOOL => {
                val = Val::BOOL(BoolVal::UNDEF);
            }
            Token::INT => {
                val = Val::INT(0);
            }
            Token::CELL => {
                val = Val::CELL(CellVal::UNDEF);
            }
            Token::EOF => {
                panic!("Unexpected EOF");
            }
            _ => {
                return None;
            }
        }
        self.index += 1;
        #[derive(PartialEq)]
        enum Prev {
            DECL,
            ID,
            COMMA,
            SEMICOLON
        }
        let mut prev = Prev::DECL;
        println!("index: {}", self.index);
        for token in self.tokens.iter().skip(self.index) {
            match token {
                Token::ID(string) => {
                    if prev == Prev::DECL || prev == Prev::COMMA {
                        vars.push(Var::from(string.clone(), val));
                    } else {
                        panic!("Var declaration error, ID is not expected!");
                    }
                    prev = Prev::ID;
                }
                Token::COMMA => {
                    if prev != Prev::ID {
                        panic!("Var declaration error, comma is not expected!");
                    }
                    prev = Prev::COMMA;
                }
                Token::SEMICOLON => {
                    if prev != Prev::ID {
                        panic!("Var declaration error, semicolon is not expected!");
                    }
                    prev = Prev::SEMICOLON;
                    self.index += 1;
                    break;
                }
                _ => {
                        panic!("Var declaration error, unexpected token {:?}!", token);
                }
            }
            self.index += 1;
        }

        if prev != Prev::SEMICOLON {
            println!("Var declaration error, semicolon is expected!");
            panic!();
        }

        return Some(vars);
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        use lexer::Token;
        let mut stmts = Vec::new();

        match &self.tokens[self.index] {
            Token::VAR | Token::BOOL | Token::INT | Token::CELL => {
                if let Some(mut vec) = self.parse_var_decl() {
                    stmts.append(&mut vec);
                }
            }
            Token::IF => {
                self.parse_if()
            }
            Token::WHILE => {

            }
            Token::ID(_) => {

            }
            Token::LOOK | Token::TEST | Token::LEFT | Token::RIGHT |
            Token::LOAD | Token::DROP | Token::FORWARD | Token::BACKWARD => {

            }
            Token::RETURN => {

            }
            _ => {
                panic!("unexpected token {:?}", self.tokens[self.index]);
            }
        }

        match &self.tokens[self.index] {
            Token::SEMICOLON => {
                self.index += 1;
            }
            _ => {
                panic!("token ; expected, {:?} found", self.tokens[self.index]);
            }
        }
        return Some(stmts);
    }

    fn parse_func(&mut self) -> bool {
        use lexer::Token;
        let mut func : Func = Func::new();

        /* FUNCTION */
        match &self.tokens[self.index] {
            Token::FUNCTION => {
                self.index += 1;
            }
            _ => {
                panic!("expected token FUNCTION, {:?} found", self.tokens[self.index]);
            }
        }

        /* ID */
        match &self.tokens[self.index] {
            Token::ID(string) => {
                func.name = string.clone();
                self.index += 1;
            }
            _ => {
                panic!("token ID expected, {:?} found", self.tokens[self.index]);
            }
        }

        /* ( */
        match &self.tokens[self.index] {
            Token::LP => {
                self.index += 1;
            }
            _ => {
                panic!("token ( expected, {:?} found", self.tokens[self.index]);
            }
        }

        /* ID */
        match &self.tokens[self.index] {
            Token::ID(string) => {
                func.param = Var::from(string.clone(), Val::new());
                self.index += 1;
            }
            _ => {
                panic!("token ID expected, {:?} found", self.tokens[self.index]);
            }
        }

        /* ) */
        match &self.tokens[self.index] {
            Token::RP => {
                self.index += 1;
            }
            _ => {
                panic!("token ) expected, {:?} found", self.tokens[self.index]);
            }
        }

        /* DO */
        match &self.tokens[self.index] {
            Token::DO => {
                self.index += 1;
            }
            _ => {
                panic!("token ) expected, {:?} found", self.tokens[self.index]);
            }
        }

        /* stmt */
        while let Some(stmt) = self.parse_stmt() {
            func.stmts.push(stmt);
        }

        /* DONE */
        match &self.tokens[self.index] {
            Token::DONE => {
                self.index += 1;
            }
            _ => {
                panic!("token ) expected, {:?} found", self.tokens[self.index]);
            }
        }

        true
    }

    fn parse_func_list(&mut self) -> bool {
        let mut n = 0;
        while self.parse_func() {
            n = n + 1;
        }
        if n > 0 {
            return true;
        }
        return false;
    }

    pub fn build_ast(&mut self) -> bool {
        use lexer::Token;
        while let Some(token) = self.lexer.get_token() {
            match token {
                Token::UNKNOWN => {
                    println!("Lexer error, unknown symbol!");
                    return false;
                }
                _ => {
                    self.tokens.push(token);
                }
            }
        }
        self.tokens.push(Token::EOF);

        let i = self.index;
        let mut n = 0;
        while let Some(mut vec) = self.parse_var_decl() {
            self.ast.vars.append(&mut vec);
            n = n + 1;
        }
        if n == 0 {
            println!("There is no global variables");
            self.index = i;
        }
        if self.parse_func_list() {
            println!("There is no funtions! Write a one!");
            return false;
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_var_decl() {
        let string = String::from("var misha, masha, jopa; bool muha;
                                   cell MIHA, appLe, tink;");
        let mut parser = Parser::new(string);
        parser.build_ast();
        println!("{:?}", parser.ast.vars);
    }
}
