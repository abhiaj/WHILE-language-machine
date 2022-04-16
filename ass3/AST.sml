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

