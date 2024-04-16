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
    cond : LogicExpr,
    stmts : Vec<Stmt>,
    eldef : Vec<Stmt>,
    elund : Vec<Stmt>
}

impl IfStmt {
    pub fn new() -> IfStmt {
        IfStmt {cond: LogicExpr::new(), stmts: Vec::new(),
                eldef : Vec::new(), elund : Vec::new()}
    }
}

#[derive(Debug)]
struct WhileStmt {
    cond : LogicExpr,
    stmts : Vec<Stmt>,
    finish : Vec<Stmt>
}

impl WhileStmt {
    pub fn new() -> WhileStmt {
        WhileStmt { cond : LogicExpr::new(), stmts : Vec::new(), finish : Vec::new() }
    }
}

#[derive(Debug)]
enum OP {
    LESS,
    MORE,
    EQUALS,
    PLUS,
    MINUS,
    XOR
}

#[derive(Debug)]
enum ArithExpr {
    EXPR((Box<ArithExpr>, Box<ArithExpr>, OP)),
    ID(Var),
    NUM(i64),
    FALSE,
    TRUE,
    EMPTY,
    EXIT,
    UNDEF
}

impl ArithExpr {
    pub fn new() -> ArithExpr {
        ArithExpr::UNDEF
    }
}

#[derive(Debug)]
struct LogicExpr {
    operation : Option<OP>,
    larex : ArithExpr,
    rarex : ArithExpr
}

impl LogicExpr {
    pub fn new() -> LogicExpr {
        LogicExpr {operation : None, larex : ArithExpr::new(), rarex : ArithExpr::new()}
    }
    pub fn from(op : Option<OP>, larex : ArithExpr, rarex :ArithExpr) -> LogicExpr {
        LogicExpr {operation: op, larex, rarex}
    }
}

#[derive(Debug)]
enum Stmt {
    IF(IfStmt),
    WHILE(WhileStmt),
    ASSGN(Var, ArithExpr),
    CALL(String, Var),
    LOAD(ArithExpr),
    DROP(ArithExpr),
    FORWARD(ArithExpr),
    BACKWARD(ArithExpr),
    RETURN,
    LOOK,
    TEST,
    LEFT,
    RIGHT,
    UNDEF
}

impl Stmt {
    pub fn new() -> Stmt {
        Stmt::UNDEF
    }
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

    fn match_token(&mut self, token : lexer::Token) {
        if self.tokens[self.index] == token {
            self.index += 1;
        } else {
            panic!("{:?} expected, {:?} found", token, self.next_token())
        }
    }

    fn next_token(&self) -> &lexer::Token {
        return &self.tokens[self.index];
    }

    fn parse_var_decl(&mut self) -> Option<Vec<Var>> {
        use lexer::Token;
        let val : Val;
        let mut vars : Vec<Var> = Vec::new();

        match self.next_token() {
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
            panic!("Var declaration error, semicolon is expected!");
        }

        return Some(vars);
    }

    fn parse_logic_expr(&mut self) -> LogicExpr {
        use lexer::Token;
        let larex;
        let rarex;
        larex = self.parse_arith_expr();
        match self.next_token() {
            Token::LESS => {
                self.index += 1;
                rarex = self.parse_arith_expr();
                return LogicExpr::from(Some(OP::LESS), larex, rarex);
            }
            Token::MORE => {
                self.index += 1;
                rarex = self.parse_arith_expr();
                return LogicExpr::from(Some(OP::MORE), larex, rarex);
            }
            Token::EQUALS => {
                self.index += 1;
                rarex = self.parse_arith_expr();
                return LogicExpr::from(Some(OP::EQUALS), larex, rarex);

            }
            _ => {
                return LogicExpr::from(None, larex, ArithExpr::new());
            }
        }

    }

    fn parse_if_stmt(&mut self) -> Option<Stmt> {
        use lexer::Token;
        let mut if_stmt = IfStmt::new();

        if_stmt.cond = self.parse_logic_expr();

        self.match_token(Token::DO);

        while let Some(stmt) = self.parse_stmt() {
            if_stmt.stmts.push(stmt);
        }

        self.match_token(Token::DONE);

        match self.next_token() {
            Token::ELDEF => {
                self.index += 1;
                while let Some(stmt) = self.parse_stmt() {
                    if_stmt.eldef.push(stmt);
                }
                self.match_token(Token::DONE);
            }
            _ => {}
        }

        match self.next_token() {
            Token::ELUND => {
                self.index += 1;
                while let Some(stmt) = self.parse_stmt() {
                    if_stmt.elund.push(stmt);
                }
                self.match_token(Token::DONE);
            }
            _ => {}
        }

        return Some(Stmt::IF(if_stmt))
    }

    fn parse_while_stmt(&mut self) -> Option<Stmt> {
        use lexer::Token;
        let mut while_stmt = WhileStmt::new();

        self.match_token(Token::WHILE);
        while_stmt.cond = self.parse_logic_expr();
        self.match_token(Token::DO);
        while let Some(stmt) = self.parse_stmt() {
            while_stmt.stmts.push(stmt);
        }
        self.match_token(Token::DONE);
        match self.next_token() {
            Token::FINISH => {
                while let Some(stmt) = self.parse_stmt() {
                    while_stmt.finish.push(stmt);
                }
            }
            _ => {}
        }
        return Some(Stmt::WHILE(while_stmt))
    }

    fn parse_arith_expr(&mut self) -> ArithExpr {
        use lexer::Token;
        let arex;

        match self.next_token().clone() {
            Token::ID(string) => {
                self.index += 1;
                arex = ArithExpr::ID(Var::from(string.clone(), Val::UNDEF));
            }
            Token::NUM(num) => {
                self.index += 1;
                arex = ArithExpr::NUM(num);
            }
            Token::FALSE => {
                self.index += 1;
                arex = ArithExpr::FALSE;
            }
            Token::TRUE => {
                self.index += 1;
                arex = ArithExpr::TRUE;
            }
            Token::EMPTY => {
                self.index += 1;
                arex = ArithExpr::EMPTY;
            }
            Token::EXIT => {
                self.index += 1;
                arex = ArithExpr::EXIT;
            }
            Token::LP => {
                self.index += 1;
                arex = self.parse_arith_expr();
                self.match_token(Token::RP);
                return arex;
            }
            Token::MINUS => {
                self.index += 1;
                arex = self.parse_arith_expr();
                return ArithExpr::EXPR((Box::new(arex), Box::new(ArithExpr::new()), OP::MINUS))
            }
            _ => {
                panic!("token {:?} unexpectedly happened", self.next_token());
            }
        }
        match self.next_token() {
            Token::PLUS => {
                self.index += 1;
                return ArithExpr::EXPR((Box::new(arex), Box::new(self.parse_arith_expr()), OP::PLUS));
            }
            Token::MINUS => {
                self.index += 1;
                return ArithExpr::EXPR((Box::new(arex), Box::new(self.parse_arith_expr()), OP::MINUS));
            }
            Token::XOR => {
                self.index += 1;
                return ArithExpr::EXPR((Box::new(arex), Box::new(self.parse_arith_expr()), OP::XOR));
            }
            _ => {
                panic!("d;lasfjo;ldj");
            }
        }
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        use lexer::Token;
        let stmt;
        let arex;
        /* if stmt is empty */
        match self.next_token() {
            Token::DONE => {
                return None
            }
            _ => {}
        }
        match self.next_token().clone() {
            Token::ID(string) => {
                self.index += 1;
                match self.next_token() {
                    Token::ASSIGN => {
                        self.index += 1;
                        arex = self.parse_arith_expr();
                        self.match_token(Token::SEMICOLON);
                        stmt = Stmt::ASSGN(Var::from(string.clone(), Val::new()), arex);
                        return Some(stmt);
                    }
                    /* function call */
                    Token::LP => {
                        self.index += 1;
                        match self.next_token() {
                            Token::ID(string2) => {
                                stmt = Stmt::CALL(string, Var::from(string2.clone(), Val::new()));
                                self.match_token(Token::RP);
                                self.match_token(Token::SEMICOLON);
                                return Some(stmt);
                            }
                            _ => {
                                panic!("expected token ID, {:?} found", self.next_token());
                            }
                        }
                    }
                    _ => {
                        panic!("unexpected token {:?}", self.next_token());
                    }
                }
            }
            Token::LOOK => {
                stmt = Stmt::LOOK;
            }
            Token::TEST => {
                stmt = Stmt::TEST;
            }
            Token::LEFT => {
                stmt = Stmt::LEFT;
            }
            Token::RIGHT => {
                stmt = Stmt::RIGHT;
            }
            Token::LOAD => {
                stmt = Stmt::LOAD(self.parse_arith_expr());
                self.index += 1;
            }
            Token::DROP => {
                stmt = Stmt::DROP(self.parse_arith_expr());
                self.index += 1;
            }
            Token::FORWARD => {
                stmt = Stmt::FORWARD(self.parse_arith_expr());
                self.index += 1;
            }
            Token::BACKWARD => {
                stmt = Stmt::BACKWARD(self.parse_arith_expr());
                self.index += 1;
            }

            Token::IF => {
                /* so we dont need check this token twice */
                self.index += 1;
                return self.parse_if_stmt()
            }
            Token::WHILE => {
                self.index += 1;
                return self.parse_while_stmt()
            }
            Token::RETURN => {
                self.index += 1;
                stmt = Stmt::RETURN;
            }
            _ => {
                panic!("unexpected token {:?}", self.next_token());
            }
        }
        self.match_token(Token::SEMICOLON);
        return Some(stmt);
    }

    fn parse_func(&mut self) -> Option<Func> {
        use lexer::Token;
        let mut func : Func = Func::new();

        /* FUNCTION */
        match self.tokens[self.index] {
            Token::FUNCTION => {
                self.index += 1;
            }
            Token::EOF => {
                return None;
            }
            _ => {
                panic!("token EOF or FUNCTION expected, {:?} found!", self.next_token());
            }
        }
        /* ID */
        match self.next_token() {
            Token::ID(string) => {
                func.name = string.clone();
                self.index += 1;
            }
            _ => {
                panic!("token ID expected, {:?} found", self.next_token());
            }
        }

        /* ( */
        self.match_token(Token::LP);

        /* ID */
        match self.next_token() {
            Token::ID(string) => {
                func.param = Var::from(string.clone(), Val::new());
                self.index += 1;
            }
            _ => {
                panic!("token ID expected, {:?} found", self.next_token());
            }
        }

        /* ) */
        self.match_token(Token::RP);

        /* var_decls */
        match self.next_token() {
            Token::VAR | Token::BOOL | Token::INT | Token::CELL => {
                while let Some(mut vec) = self.parse_var_decl() {
                    func.vars.append(&mut vec);
                }
            }
            _ => {}
        }

        /* DO */
        self.match_token(Token::DO);

        /* stmt */
        while let Some(stmt) = self.parse_stmt() {
            func.stmts.push(stmt);
        }

        /* DONE */
        self.match_token(Token::DONE);

        Some(func)
    }

    fn parse_func_list(&mut self) -> Vec<Func> {
        let mut funcs = Vec::new();
        while let Some(func) = self.parse_func() {
            funcs.push(func);
        }
        return funcs;
    }

    pub fn build_ast(&mut self) {
        use lexer::Token;
        while let Some(token) = self.lexer.get_token() {
            match token {
                Token::UNKNOWN => {
                    panic!("Lexer error, unknown symbol!");
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
            println!("There is no global variables, fyi");
            self.index = i;
        }
        self.ast.funcs = self.parse_func_list();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::Read;

    #[test]
    fn test_var_decl() {
        let string = Vec::from("var first;");
        let mut file = File::open("test_program1.imbaa").unwrap();
        let mut file_prog = String::new();
        for byte in string {

        }
        file.read_to_string(&mut file_prog).unwrap();
        println!("{}|length {}", file_prog.trim(), file_prog.trim().len());
        let mut parser = Parser::new(file_prog);
        parser.build_ast();
        println!("{:?}", parser.ast.vars);
    }
}
