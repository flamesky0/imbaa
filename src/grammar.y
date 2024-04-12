%token FUNCTION
%token ID
%token NUM
%token DO
%token DONE
%token IF
%token WHILE
%token LOOK
%token VAR
%token BOOL
%token INT
%token CELL
%token RETURN
%token LEFT
%token RIGHT
%token LOAD
%token DROP
%token TEST
%token FORWARD
%token BACKWARD
%token ASSGN
%token FINISH
%token ELDEF
%token ELUND

%%

program:
		var_decls
	|	func_list
       	;

var_decls: /* empty */
	| 	var_decl ';'
	|	var_decl  var_decls
	;

func_list:
	 	func
	| 	func func_list
	;

func:
    		FUNCTION ID '(' ID ')' DO stmts DONE
	;

stmts:
     		stmt
	|	stmt stmts
	;

stmt:
		var_decl ';'
    	|	IF expr DO stmts DONE
    	|	IF expr DO stmts DONE ELDEF DO stmts DONE
    	|	IF expr DO stmts DONE ELUND DO stmts DONE
    	|	IF expr DO stmts DONE ELDEF DO stmts DONE ELUND DO stmts DONE
	|	WHILE expr DO stmts DONE
	|	WHILE expr DO stmts DONE FINISH stmts DONE
	|	ID ASSGN arex ';'
	|	expr ';'
	|	ID '(' ID ')' ';' // function call, values transfer over variable only
	| 	RETURN ';'
	;

var_decl:
		VAR var_list
	|	BOOL var_list
	|	INT var_list
	|	CELL var_list
	;

var_list:
		ID
	|	ID ',' var_list
	;

expr:
		LOOK
	|	TEST
	|	LEFT
	|	RIGHT
	|	LOAD arex
	|	DROP arex
	|	FORWARD arex
	|	BACKWARD arex
	|	lgex
	;

lgex:
	|	lgex '>' arex
	|	lgex '<' arex
	|	lgex '=' arex
	|	arex
	;

arex:
    		factor
	|	factor '+' arex
	|	factor '-' arex
	|	factor '^' arex // it is XOR
	|	'-' arex
	;

factor:
      		'(' arex ')'
	|	ID
	|	NUM
	;
