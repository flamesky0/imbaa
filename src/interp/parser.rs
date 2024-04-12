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

#[derive(Debug)]
struct Var {
    name : String,
    value : Val
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
    vars : Vec<Var>,
    stmts : Vec<Stmt>
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

    fn parse_var_decl(&mut self) -> bool {
        use lexer::Token;
        let val : Val;
        let mut var : Var;
        if self.index >= self.tokens.len() {
            return false;
        }
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
            _ => {
                return false;
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
                        var = Var {name : string.clone(), value: val};
                        self.ast.vars.push(var);
                    } else {
                        println!("Var declaration error, ID is not expected!");
                        panic!()
                    }
                    prev = Prev::ID;
                }
                Token::COMMA => {
                    if prev != Prev::ID {
                        println!("Var declaration error, comma is not expected!");
                        panic!()
                    }
                    prev = Prev::COMMA;
                }
                Token::SEMICOLON => {
                    if prev != Prev::ID {
                        println!("Var declaration error, semicolon is not expected!");
                        panic!()
                    }
                    prev = Prev::SEMICOLON;
                    self.index += 1;
                    break;
                }
                _ => {
                        println!("Var declaration error, unexpected token {:?}!", token);
                        panic!()
                }
            }
            self.index += 1;
        }

        if prev != Prev::SEMICOLON {
            println!("Var declaration error, semicolon is expected!");
            panic!();
        }

        return true;
    }

    fn parse_func_list(&mut self) -> bool {
        false
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

        let i = self.index;
        let mut n = 0;
        while self.parse_var_decl() {
            n = n + 1;
        }
        if n == 0 {
            println!("There is no global variables");
            self.index = i;
        }
        if !self.parse_func_list() {
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
