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


    
    
    
    fun listToPostfix []         = ""
    |   listToPostfix (x::xs)    =  (x) ^ " " ^ (listToPostfix(xs));
    

    fun ExpToPostfix (INTEGER(int)) = Int.toString(int)(*^" INTEGER"*)
        | ExpToPostfix (ID(x)) = x^" ID"
        | ExpToPostfix (PLUS(exp1, exp2)) = ExpToPostfix(exp1)^" "^ExpToPostfix(exp2)^" PLUS"
        | ExpToPostfix (MINUS(exp1, exp2)) = ExpToPostfix(exp1)^" "^ExpToPostfix(exp2)^" MINUS"
        | ExpToPostfix (TIMES(exp1, exp2)) = ExpToPostfix(exp1)^" "^ExpToPostfix(exp2)^" TIMES"
        | ExpToPostfix (DIV(exp1, exp2)) = ExpToPostfix(exp1)^" "^ExpToPostfix(exp2)^" DIV"
        | ExpToPostfix (MOD(exp1, exp2)) = ExpToPostfix(exp1)^" "^ExpToPostfix(exp2)^" MOD"
        | ExpToPostfix (NEG(exp)) = ExpToPostfix(exp)^" NEG"

        | ExpToPostfix TT = "TT"
        | ExpToPostfix FF = "FF"
        | ExpToPostfix (IDB(x)) = x^" IDB"

        | ExpToPostfix (AND(exp1, exp2)) = ExpToPostfix(exp1)^" "^ExpToPostfix(exp2)^" AND"
        | ExpToPostfix (OR(exp1, exp2)) = ExpToPostfix(exp1)^" "^ExpToPostfix(exp2)^" OR"
        | ExpToPostfix (NOT(exp)) = ExpToPostfix(exp)^" NOT"

        | ExpToPostfix (GT(exp1, exp2)) = ExpToPostfix(exp1)^" "^ExpToPostfix(exp2)^" GT"
        | ExpToPostfix (LT(exp1, exp2)) = ExpToPostfix(exp1)^" "^ExpToPostfix(exp2)^" LT"
        | ExpToPostfix (GTE(exp1, exp2)) = ExpToPostfix(exp1)^" "^ExpToPostfix(exp2)^" GTE"
        | ExpToPostfix (LTE(exp1, exp2)) = ExpToPostfix(exp1)^" "^ExpToPostfix(exp2)^" LTE"
        | ExpToPostfix (EQ(exp1, exp2)) = ExpToPostfix(exp1)^" "^ExpToPostfix(exp2)^" EQ"
        | ExpToPostfix (NEQ(exp1, exp2)) = ExpToPostfix(exp1)^" "^ExpToPostfix(exp2)^" NEQ";


    


    fun CommToPostfix (SET(str, exp)) = str^" "^ExpToPostfix(exp)^" SET"
        | CommToPostfix (READ(str)) = str^" READ"
        | CommToPostfix (WRITE(exp)) = ExpToPostfix(exp)^" WRITE"
        | CommToPostfix (ITE(exp, CommL1, CommL2)) = ExpToPostfix(exp)^" IF "^CommListToPostfix(CommL1)
                        ^" THEN "^CommListToPostfix(CommL2)^" ELSE"
        | CommToPostfix (WHILE(exp, CommL)) = "WHILE "^ ExpToPostfix(exp)^" DO "^CommListToPostfix(CommL)^" ENDWH"

    and CommListToPostfix []         = ""
    |   CommListToPostfix (x::xs)    =  CommToPostfix(x) ^ " " ^ (CommListToPostfix(xs));
        

    fun DeclToPostfix (DECL(strL, INT)) = listToPostfix(strL)^" INT DECL"
        | DeclToPostfix (DECL(strL, BOOL)) = listToPostfix(strL)^" BOOL DECL";

    fun DeclListToPostfix []         = ""
        |   DeclListToPostfix (x::xs)    =  DeclToPostfix(x) ^ " " ^ DeclListToPostfix(xs);


    fun postfix (PROG(x, DecL, CommL)) = CommListToPostfix(CommL);



end

