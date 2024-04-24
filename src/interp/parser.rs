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
    type_ : Val
}

impl Var {
    pub fn new() -> Var {
        Var {name : String::new(), type_ : Val::new()}
    }

    pub fn from(name : String, type_ : Val) -> Var {
        Var {name, type_}
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
pub struct Ast {
    vars : Vec<Var>,
    funcs : Vec<Func>
}

pub struct Parser <'a> {
    lexer : std::iter::Peekable<lexer::Lexer<'a>>,
    ast : Ast,
}

use lexer::Lexer;

impl <'a> Parser <'a>{
    pub fn new(string : &'a String) -> Parser<'a> {
        Parser {
                lexer : Lexer::new(&string).into_iter().peekable(),
                ast : Ast {vars: Vec::new(), funcs : Vec::new()},
        }
    }

    fn match_token(&mut self, token : lexer::Token) -> Result<(), String> {
        match self.lexer.peek() {
            Some(next_token) => {
                if token == *next_token {
                    self.lexer.next();
                    return Ok(())
                } else {
                    return Err(format!("expected {:?}, found {:?}", token, next_token).to_string());
                }
            },
            None => {
                return Err("unexpected EOF".to_string())
            }
        }
    }

    fn parse_var_decl(&mut self) -> Result<Option<Vec<Var>>, String> {
        println!("parse_val_decl");
        use lexer::Token;
        let val : Val;
        let mut vars : Vec<Var> = Vec::new();

        match self.lexer.peek().unwrap_or_default() {
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
                return Ok(None);
            }
        }
        self.lexer.next();
        #[derive(PartialEq)]
        enum Prev {
            DECL,
            ID,
            COMMA,
            SEMICOLON
        }
        let mut prev = Prev::DECL;
        for token in &mut self.lexer {
            match token {
                Token::ID(string) => {
                    if prev == Prev::DECL || prev == Prev::COMMA {
                        vars.push(Var::from(string, val));
                    } else {
                        return Err("Var declaration error, ID is not expected!".to_string());
                    }
                    prev = Prev::ID;
                }
                Token::COMMA => {
                    if prev != Prev::ID {
                        return Err("Var declaration error, comma is not expected!".to_string());
                    }
                    prev = Prev::COMMA;
                }
                Token::SEMICOLON => {
                    if prev != Prev::ID {
                        return Err("Var declaration error, semicolon is not expected!".to_string());
                    }
                    prev = Prev::SEMICOLON;
                    break;
                }
                _ => {
                        return Err(format!("Var declaration error, unexpected token {:?}!", token).to_string());
                }
            }
        }

        if prev != Prev::SEMICOLON {
            return Err("Var declaration error, semicolon is expected!".to_string());
        }

        return Ok(Some(vars));
    }

    fn parse_logic_expr(&mut self) -> Result<LogicExpr, String> {
        println!("parse_logic_expr");
        use lexer::Token;
        let larex;
        let rarex;
        larex = self.parse_arith_expr()?;
        match self.lexer.peek().unwrap_or_default() {
            Token::LESS => {
                self.lexer.next();
                rarex = self.parse_arith_expr()?;
                return Ok(LogicExpr::from(Some(OP::LESS), larex, rarex));
            }
            Token::MORE => {
                self.lexer.next();
                rarex = self.parse_arith_expr()?;
                return Ok(LogicExpr::from(Some(OP::MORE), larex, rarex));
            }
            Token::EQUALS => {
                self.lexer.next();
                rarex = self.parse_arith_expr()?;
                return Ok(LogicExpr::from(Some(OP::EQUALS), larex, rarex));

            }
            _ => {
                return Ok(LogicExpr::from(None, larex, ArithExpr::new()));
            }
        }

    }

    fn parse_if_stmt(&mut self) -> Result<Option<Stmt>, String> {
        println!("parse_if_stmt");
        use lexer::Token;
        let mut if_stmt = IfStmt::new();

        if_stmt.cond = self.parse_logic_expr()?;

        self.match_token(Token::DO)?;

        while let Ok(Some(stmt)) = self.parse_stmt() {
            if_stmt.stmts.push(stmt);
        }

        self.match_token(Token::DONE)?;

        match self.lexer.peek().unwrap_or_default() {
            Token::ELDEF => {
                self.lexer.next();
                self.match_token(Token::DO)?;
                while let Ok(Some(stmt)) = self.parse_stmt() {
                    if_stmt.eldef.push(stmt);
                }
                self.match_token(Token::DONE)?;
            }
            _ => {}
        }

        match self.lexer.peek().unwrap_or(&Token::EOF) {
            Token::ELUND => {
                self.lexer.next();
                self.match_token(Token::DO)?;
                while let Ok(Some(stmt)) = self.parse_stmt() {
                    if_stmt.elund.push(stmt);
                }
                self.match_token(Token::DONE)?;
            }
            _ => {}
        }

        return Ok(Some(Stmt::IF(if_stmt)))
    }

    fn parse_while_stmt(&mut self) -> Result<Option<Stmt>, String> {
        println!("parse_while_stmt");
        use lexer::Token;
        let mut while_stmt = WhileStmt::new();

        while_stmt.cond = self.parse_logic_expr()?;
        self.match_token(Token::DO)?;
        while let Ok(Some(stmt)) = self.parse_stmt() {
            while_stmt.stmts.push(stmt);
        }
        self.match_token(Token::DONE)?;
        match self.lexer.peek().unwrap_or(&Token::EOF) {
            Token::FINISH => {
                self.match_token(Token::FINISH)?;
                while let Ok(Some(stmt)) = self.parse_stmt() {
                    while_stmt.finish.push(stmt);
                }
                self.match_token(Token::DONE)?;
            }
            _ => {}
        }
        return Ok(Some(Stmt::WHILE(while_stmt)))
    }

    fn parse_arith_expr(&mut self) -> Result<ArithExpr, String> {
        println!("parse_arith_stmt");
        use lexer::Token;
        let arex;

        match self.lexer.next() {
            Some(token) => {
                match token {
                    Token::ID(string) => {
                        arex = ArithExpr::ID(Var::from(string, Val::new()));
                    }
                    Token::NUM(num) => {
                        arex = ArithExpr::NUM(num);
                    }
                    Token::FALSE => {
                        arex = ArithExpr::FALSE;
                    }
                    Token::TRUE => {
                        arex = ArithExpr::TRUE;
                    }
                    Token::EMPTY => {
                        arex = ArithExpr::EMPTY;
                    }
                    Token::EXIT => {
                        arex = ArithExpr::EXIT;
                    }
                    Token::LP => {
                        arex = self.parse_arith_expr()?;
                        self.match_token(Token::RP)?;
                    }
                    Token::MINUS => {
                        arex = self.parse_arith_expr()?;
                        return Ok(ArithExpr::EXPR((Box::new(arex), Box::new(ArithExpr::new()), OP::MINUS)))
                    }
                    x => {
                        return Err(format!("token {:?} unexpectedly happened", x).to_string());
                    }
                }
            }
            None => {
                return Err(format!("token {:?} unexpectedly happened", self.lexer.peek().unwrap_or_default()).to_string());
            }
        }
        match self.lexer.peek().unwrap_or_default() {
            Token::PLUS => {
                self.lexer.next();
                return Ok(ArithExpr::EXPR((Box::new(arex), Box::new(self.parse_arith_expr()?), OP::PLUS)));
            }
            Token::MINUS => {
                self.lexer.next();
                return Ok(ArithExpr::EXPR((Box::new(arex), Box::new(self.parse_arith_expr()?), OP::MINUS)));
            }
            Token::XOR => {
                self.lexer.next();
                return Ok(ArithExpr::EXPR((Box::new(arex), Box::new(self.parse_arith_expr()?), OP::XOR)));
            }
            _ => {
                return Ok(arex);
            }
        }
    }

    fn parse_func_call(&mut self, name : String) -> Result<Option<Stmt>, String> {
        use lexer::Token;
        let stmt;
        match self.lexer.next() {
            Some(token) => {
                match token {
                    Token::ID(string2) => {
                        stmt = Stmt::CALL(name, Var::from(string2, Val::new()));
                        self.match_token(Token::RP)?;
                        self.match_token(Token::SEMICOLON)?;
                        return Ok(Some(stmt));
                    }
                    x => {
                        return Err(format!("expected token ID, {:?} found", x).to_string());
                    }

                }
            }
            None => {
                return Err("unexpected token EOF".to_string());
            }
        }
    }

    fn parse_assign_or_func_call(&mut self, name : String) -> Result<Option<Stmt>, String> {
        use lexer::Token;
        let arex;
        let stmt;
        match self.lexer.next() {
            Some(token) => {
                match token {
                    Token::ASSIGN => {
                        arex = self.parse_arith_expr()?;
                        self.match_token(Token::SEMICOLON)?;
                        stmt = Stmt::ASSGN(Var::from(name, Val::new()), arex);
                        return Ok(Some(stmt));
                    }
                    /* function call */
                    Token::LP => {
                        return self.parse_func_call(name);
                    }
                    x => {
                        return Err(format!("unexpected token {:?}", x).to_string());
                    }
                }
            }
            None => {
                return Err("unexpected token EOF".to_string());
            }
        }
    }

    fn parse_stmt(&mut self) -> Result<Option<Stmt>, String> {
        use lexer::Token;
        let stmt;
        /* if stmt is empty */
        println!("parse_stmt");
        match self.lexer.peek().unwrap_or_default() {
            Token::DONE => {
                return Ok(None);
            }
            _ => {}
        }
        match self.lexer.next() {
            Some(token) => {
                match token {
                    Token::ID(string) => {
                        return self.parse_assign_or_func_call(string);
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
                        stmt = Stmt::LOAD(self.parse_arith_expr()?);
                    }
                    Token::DROP => {
                        stmt = Stmt::DROP(self.parse_arith_expr()?);
                    }
                    Token::FORWARD => {
                        stmt = Stmt::FORWARD(self.parse_arith_expr()?);
                    }
                    Token::BACKWARD => {
                        stmt = Stmt::BACKWARD(self.parse_arith_expr()?);
                    }
                    Token::IF => {
                        /* so we dont need check this token twice */
                        return self.parse_if_stmt()
                    }
                    Token::WHILE => {
                        return self.parse_while_stmt()
                    }
                    Token::RETURN => {
                        stmt = Stmt::RETURN;
                    }
                    x => {
                        return Err(format!("unexpected token {:?}", x).to_string());
                    }
                }
            }
            None => {
                return Err("unexpected token EOF".to_string());
            }
        }
        self.match_token(Token::SEMICOLON)?;
        return Ok(Some(stmt));
    }

    fn parse_func(&mut self) -> Result<Option<Func>, String> {
        use lexer::Token;
        let mut func : Func = Func::new();

        /* FUNCTION */
        println!("parse_func");
        match self.lexer.peek().unwrap_or_default() {
            Token::FUNCTION => {
                self.lexer.next();
            }
            Token::EOF => {
                return Ok(None);
            }
            x => {
                return Err(format!("token EOF or FUNCTION expected, {:?} found!", x).to_string());
            }
        }
        /* ID */
        match self.lexer.next() {
            Some(token) => {
                match token {
                    Token::ID(string) => {
                        func.name = string;
                    }
                    x => {
                        return Err(format!("token ID expected, {:?} found!", x).to_string());
                    }
                }
            }
            None => {
                return Err("token ID expected, EOF found!".to_string());
            }
        }

        /* ( */
        self.match_token(Token::LP)?;

        /* ) */
        self.match_token(Token::RP)?;

        /* var_decls */
        match self.lexer.peek().unwrap_or_default() {
            Token::VAR | Token::BOOL | Token::INT | Token::CELL => {
                while let Ok(Some(mut vec)) = self.parse_var_decl() {
                    func.vars.append(&mut vec);
                }
            }
            _ => {}
        }

        /* DO */
        self.match_token(Token::DO)?;

        /* stmt */
        while let Ok(Some(stmt)) = self.parse_stmt() {
            func.stmts.push(stmt);
        }

        /* DONE */
        self.match_token(Token::DONE)?;

        Ok(Some(func))
    }

    fn parse_func_list(&mut self) -> Result<Vec<Func>, String> {
        println!("parse_func_list");
        let mut funcs = Vec::new();
        while let Ok(Some(func)) = self.parse_func() {
            funcs.push(func);
        }
        return Ok(funcs);
    }

    pub fn build_ast(mut self) -> Result<Ast, String> {
        let mut n = 0;
        while let Ok(Some(mut vec)) = self.parse_var_decl() {
            self.ast.vars.append(&mut vec);
            n = n + 1;
        }
        if n == 0 {
            println!("There is no global variables, fyi");
        }
        self.ast.funcs = self.parse_func_list()?;
        return Ok(self.ast);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::Read;

    #[test]
    fn parser() {
        //let string = Vec::from("var first;");
        let mut file = File::open("test_program1.imbaa").unwrap();
        let mut file_prog = String::new();
        file.read_to_string(&mut file_prog).unwrap();
        println!("-------PROGRAM---------");
        print!("{}", file_prog);
        println!("-------PROGRAM---------");
        let parser = Parser::new(&file_prog);
        match parser.build_ast() {
            Ok(ast) => {
                println!("Ok");
            }
            Err(string) => {
                println!("{}", string);
            }
        }
        //println!("{:?}", parser.ast.vars);
    }
}
