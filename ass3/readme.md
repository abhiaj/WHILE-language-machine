# 1. How to run

Using ml-yacc and ml-lex making a compiler for while programming language.
Tools needed:

$sudo apt update
$sudo apt install smlnj ml-yacc ml-lex ml-burg ml-lpt
$sudo apt install ml-ulex

How to run:
1. $ sml while_ast.sml test


FIles:
1. prog.lex - lexer file for a subset of WHILE language in ml-lex.
2. prog.grm - grammer file for a subset of WHILE language.
3. AST.sml - datatype file for the parser.grm.
4. test - a test file containing WHILE language syntax code for checking the compiler.
5. while_ast.sml - driver code for taking file through terminal, setting CM and calling parser
6. parse.sml - parser code for gluing lexer and yacc




# 2. Context Free Grammar
- The words in CAPITAL are terms/tokens/Terminals from lexer, others are non-terms/non-terminals
- I had to modify the EBNF given in slides as that was ambiguous and was giving R/R and R/S errors in yacc.
- The modifications are mostly around the fact that expressions have been clustered into one chain of rule while parsing. Syntax Direction Translation has been used to classfiy expressions into IEXP or BEXP.

## BEGIN

start: PROGRAM IDENTIFIER DCOLON declarations LCPAREN commandlist RCPAREN  

declarations: declaration declarations   

| declaration

declaration: VAR variablelist COLON INT SCOLON     
			| VAR variablelist COLON BOOL SCOLON     

variablelist: variable COMMA variablelist	
| variable 							



commandlist: command SCOLON commandlist   

			| 		command	

command: variable ASSIGN expression    

			| READ variable	

			| WRITE expression	

			| IF expression THEN LCPAREN commandlist RCPAREN ELSE LCPAREN commandlist RCPAREN ENDIF   

    		| WHILE expression DO LCPAREN commandlist RCPAREN ENDWH        

expression: or_exp 

or_exp: and_exp OR or_exp 
			| and_exp 

and_exp: eq_exp AND and_exp 
			| eq_exp 

eq_exp: rel_exp EQ eq_exp 
			| rel_exp NEQ eq_exp 
			| rel_exp 
	
rel_exp: add_exp GT rel_exp 
			| add_exp GTE rel_exp 
			| add_exp LT rel_exp 
			| add_exp LTE rel_exp 
			| add_exp 

add_exp: mult_exp PLUS add_exp 
			| mult_exp MINUS add_exp 
			| mult_exp 

mult_exp: unary_exp MUL mult_exp  
			| unary_exp DIV mult_exp  
			| unary_exp MOD mult_exp  
			| unary_exp  

unary_exp: NEG unary_exp 
			| NOT unary_exp 
			| primary_exp 

primary_exp: IDENTIFIER 
			| NUMBER  
			| TRUE  
			| FALSE 
			| LPAREN expression RPAREN 

variable: IDENTIFIER                  			(IDENTIFIER)


## END


# 3. AST datatype definition

structure AST =
struct

  type VARIABLE = string

  datatype IEXP = INTEGER of int 
          | ID of VARIABLE
          | PLUS of IEXP *IEXP
          | MINUS of IEXP * IEXP
          | TIMES of IEXP * IEXP
          | DIV of IEXP * IEXP
          | MOD of IEXP * IEXP
          | NEG of IEXP;
          

  datatype BEXP = TT
          | FF
          | IDB of VARIABLE

          | AND of BEXP * BEXP
          | OR of BEXP * BEXP
          | NOT of BEXP

          | GT of IEXP * IEXP
          | LT of IEXP * IEXP
          | GTE of IEXP * IEXP
          | LTE of IEXP * IEXP
          | EQ of IEXP * IEXP
          | NEQ of IEXP * IEXP

          | GTB of BEXP * BEXP
          | LTB of BEXP * BEXP
          | GTEB of BEXP * BEXP
          | LTEB of BEXP * BEXP
          | EQB of BEXP * BEXP
          | NEQB of BEXP * BEXP;

  datatype EXP = IEXP of EXP | BEXP of EXP
          | INTEGER of int 
          | ID of VARIABLE
          | PLUS of EXP * EXP
          | MINUS of EXP * EXP
          | TIMES of EXP * EXP
          | DIV of EXP * EXP
          | MOD of EXP * EXP
          | NEG of EXP

          | TT
          | FF
          | IDB of VARIABLE

          | AND of EXP * EXP
          | OR of EXP * EXP
          | NOT of EXP

          | GT of EXP * EXP
          | LT of EXP * EXP
          | GTE of EXP * EXP
          | LTE of EXP * EXP
          | EQ of EXP * EXP
          | NEQ of EXP * EXP

          | ERROR;

  datatype COMMAND = SET of string * EXP
          (* | SET2 of string * BEXP *)
          | READ of string 
          | WRITE of EXP
          | ITE of EXP * COMMAND list * COMMAND list
          | WHILE of EXP * COMMAND list
          | ERROR2;

  datatype TYPE = INT | BOOL;

  datatype DECLARATION = DECL of string list * TYPE;


  datatype PROGRAM = PROG of string * DECLARATION list * COMMAND list;


  exception ScanError;
  exception ParseError;



end

------------------

# 4. Syntax-directed translation
- ## Auxiliary functions and Data
- 1. Hashmap structure for storing variables along wiht their types (1 for int, 0 for bool) and doing type checking. This is the backbone of the entire semantic checking as the type is used to define int and bool variables and then further used in different expressions to classify an experssion as int or bool exp

- - 1. strtbl for structure of hashmaps
- - 2. t1 for definifng hashmap
- - 3. check function for finding if the key exists
- - 4. insert_list function for inserting variables as keys


- 2. Using hashmap to classify variables as ID (int variables) or IDB (bool variables)
- 3. Defining check_iexp and check_bexp functions to check whether an expression is int or bool.
- - 1. used SML pattern checcking.
- - 2. IEXP expressions are those that are arithmetic (exact definition in prog.grm) and output/evaluate to an int
- - 3. BEXP expressions evaluate to a bool
- - 4. BEXP relOp BEXP is supported.
- - 5. Exiting from the program if there is a semantic error. For example (3+2+tt raises error as bool+int is not allowed). Also shows the line number where there was a semantic error. The line number is shown using payload of tokens from lex.

- ## Example 

if check_iexp2(mult_exp, add_exp)=true then AST.PLUS(mult_exp, add_exp) else (exit_prog PLUSleft; AST.ERROR)

In this example 
- check_iexp2 check if the two expressions, mult_exp and add_exp are of type iexp, if yes then we proceed
- if both are not iexp, then we go to exit_prog PLUSleft. PLUSleft is the line numebr and payload of PLUS token. This is used to show which line has an error.
- AST.ERROR is datatype for error defined in AST.sml. Although it acts as dummy as program exits in exit_prog.

I am not putitng all other SDT here as it will be cluttered and they are similar in logic to the above example. You can find them in the prog.grm file.


## All functions/vals for Hahsmaps
(* Hashmap for storing the list of declared variables *)
structure StrTbl = HashTableFn(
struct
	type hash_key = string
	val hashVal = HashString.hashString
	val sameKey = (op=) : string * string -> bool
end);

val t1 : int StrTbl.hash_table = StrTbl.mkTable(100, Fail "Not found");

(* check function to check if string is in hashmap or not
if it is hashmap, 1 denotes INT var and 0 denotes BOOL var *)
val err_msg = fn variable => TextIO.output(TextIO.stdOut, "PARSE error\n"^"variable "^variable^" not defined\n\n\n")
val check = fn variable => case ( StrTbl.find t1 variable ) of 
				SOME 1 => 1
				| SOME 0 => 0
				| _ => (err_msg(variable); OS.Process.exit(OS.Process.failure); 2)

(* 1 is int, 0 is bool *)
(* function to insert list of variables from declaration to hashmap *)
fun insert_list (var::list, t) = ( StrTbl.insert t1 (var,t); insert_list (list, t) )
				| insert_list ([], t) = true;


## All functions/vals for SDT
(* function to check if an expresssion is intexpression *)
fun check_iexp (AST.IEXP v) = true
				| check_iexp (AST.TIMES (v, x)) = true
				| check_iexp (AST.PLUS (v, x)) = true
				| check_iexp (AST.MINUS (v, x)) = true
				| check_iexp (AST.MOD (v, x)) = true
				| check_iexp (AST.DIV (v, x)) = true
				| check_iexp (AST.INTEGER v) = true
				| check_iexp (AST.ID v) = true
				| check_iexp (AST.NEG v) = true
				| check_iexp _  = false

fun check_iexp2 (e1, e2) = (check_iexp (e1) andalso check_iexp (e2) )


(* function to check if an expresssion is boolexpression *)
fun check_bexp (AST.BEXP v) = true
				| check_bexp (AST.GT (v, x)) = true
				| check_bexp (AST.LT (v, x)) = true
				| check_bexp (AST.GTE (v, x)) = true
				| check_bexp (AST.LTE (v, x)) = true
				| check_bexp (AST.EQ (v, x)) = true
				| check_bexp (AST.NEQ (v, x)) = true

				| check_bexp (AST.AND (v, x)) = true
				| check_bexp (AST.OR (v, x)) = true

				| check_bexp (AST.NOT v) = true
				
				| check_bexp (AST.IDB v) = true
				| check_bexp (AST.TT ) = true
				| check_bexp (AST.FF ) = true
				| check_bexp _  = false

fun check_bexp2 (e1, e2) = check_bexp (e1) andalso check_bexp (e2) 



fun check_i2_b2 (e1, e2) = ( check_bexp (e1) andalso check_bexp (e2) ) 
				orelse ( check_iexp (e1) andalso check_iexp (e2)  )


(* function to exit the program abruptly in case of a semantic error *)
val exit_prog = fn v => ( TextIO.output(TextIO.stdOut, "\n\n Line: [" ^ Int.toString v ^ "] INvalid semantics, , exitinggggg.....\n\n "); OS.Process.exit(OS.Process.failure); v)


# 5. Other Design Decisions
- Extensive error handlign has been done
- LexerError while scanning gives exact line number and column number while giving scanning error
- ParserError handling when there are undefined variables, type checking errors, or any type mismatch including assignment.
- All the semantic errors have been covered with proper SDT.

# 6. Other Implementation Decisions
- Taking input from command line and printing on terminal, makes it very cnvenient to take input, run and debug
- To run, use "sml while_ast.sml filename"

# 7. Acknowledgements
1. https://www.smlnj.org/doc/errors.html
2. https://piazza.com/class_profile/get_resource/hpo8fqgcnhr585/htsrltcqhh7ez
3. https://www.lysator.liu.se/c/ANSI-C-grammar-y.html
4. smlfamily.github.io
5. http://rogerprice.org/



