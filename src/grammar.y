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

/*
	THIS GRAMMAR IS NOT USED IN PROJECT IN ANY WAY,
	IT'S PURPOSE TO BE SANE BISON-CHECKED REFERENCE GRAMMAR
	TO HELP THE AUTHOR IN BUILDING PARSER
*/

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
    		FUNCTION ID '(' ')' var_decls DO stmts DONE
	;

stmts:
     		stmt
	|	stmt stmts
	;

stmt:
    		IF lgex DO stmts DONE
    	|	IF lgex DO stmts DONE ELDEF DO stmts DONE
    	|	IF lgex DO stmts DONE ELUND DO stmts DONE
    	|	IF lgex DO stmts DONE ELDEF DO stmts DONE ELUND DO stmts DONE
	|	WHILE lgex DO stmts DONE
	|	WHILE lgex DO stmts DONE FINISH stmts DONE
	|	ID ASSGN arex ';'
	|	command ';'
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

command:
       		LOOK
	|	TEST
	|	LEFT
	|	RIGHT
	|	LOAD arex
	|	DROP arex
	|	FORWARD arex
	|	BACKWARD arex
       	;

lgex:
	|	agex '>' lrex
	|	agex '<' lrex
	|	agex '=' lrex
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
	|	FALSE
	|	TRUE
	|	EMPTY
	|	EXIT
	;
