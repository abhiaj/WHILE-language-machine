functor CLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : C_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* Keyword management *)

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

fun exist (_, []) = false
    | exist (x, y::ys) = x = y orelse exist (x, ys)
val keywords = ["program", "var", "int", "bool", "if", "then", "else", "endif", "while",
		"do", "endwh", "tt", "ff", "read", "write"]
val check_ident = fn variable => if exist (variable, keywords) then (TextIO.output(TextIO.stdOut, "\n\nKeywords cant be used as identifiers. Exiting\n\n\n"); OS.Process.exit(OS.Process.failure);~1)
								else 1

(* 1 is int, 0 is bool *)
(* function to insert list of variables from declaration to hashmap *)
fun insert_list (var::list, t) = ( StrTbl.insert t1 (var,t); insert_list (list, t) )
				| insert_list ([], t) = true;



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

val exit_prog2 = fn (v:int) => (TextIO.output(TextIO.stdOut, Int.toString v); 2)



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\003\000\000\000\
\\001\000\003\000\006\000\000\000\
\\001\000\004\000\022\000\000\000\
\\001\000\006\000\047\000\007\000\046\000\000\000\
\\001\000\009\000\011\000\000\000\
\\001\000\009\000\084\000\000\000\
\\001\000\009\000\085\000\000\000\
\\001\000\009\000\092\000\000\000\
\\001\000\010\000\025\000\000\000\
\\001\000\010\000\088\000\000\000\
\\001\000\010\000\089\000\000\000\
\\001\000\010\000\094\000\000\000\
\\001\000\011\000\023\000\000\000\
\\001\000\013\000\067\000\000\000\
\\001\000\014\000\091\000\000\000\
\\001\000\015\000\095\000\000\000\
\\001\000\017\000\066\000\000\000\
\\001\000\018\000\090\000\000\000\
\\001\000\019\000\041\000\023\000\040\000\024\000\039\000\025\000\038\000\
\\040\000\005\000\041\000\037\000\042\000\036\000\000\000\
\\001\000\020\000\083\000\000\000\
\\001\000\037\000\024\000\000\000\
\\001\000\037\000\068\000\000\000\
\\001\000\037\000\069\000\000\000\
\\001\000\040\000\005\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\005\000\009\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\008\000\021\000\000\000\
\\104\000\040\000\005\000\000\000\
\\105\000\000\000\
\\106\000\012\000\020\000\016\000\019\000\038\000\018\000\039\000\017\000\
\\040\000\005\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\021\000\062\000\000\000\
\\113\000\022\000\061\000\000\000\
\\114\000\022\000\061\000\000\000\
\\115\000\030\000\060\000\031\000\059\000\000\000\
\\116\000\030\000\060\000\031\000\059\000\000\000\
\\117\000\026\000\058\000\027\000\057\000\028\000\056\000\029\000\055\000\000\000\
\\118\000\026\000\058\000\027\000\057\000\028\000\056\000\029\000\055\000\000\000\
\\119\000\026\000\058\000\027\000\057\000\028\000\056\000\029\000\055\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\032\000\054\000\033\000\053\000\034\000\052\000\035\000\051\000\
\\036\000\050\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\"
val actionRowNumbers =
"\001\000\024\000\002\000\068\000\
\\027\000\027\000\005\000\032\000\
\\026\000\034\000\031\000\003\000\
\\013\000\021\000\009\000\019\000\
\\024\000\019\000\019\000\032\000\
\\004\000\019\000\034\000\025\000\
\\062\000\059\000\055\000\052\000\
\\047\000\044\000\042\000\040\000\
\\037\000\063\000\019\000\064\000\
\\019\000\066\000\065\000\019\000\
\\036\000\017\000\014\000\030\000\
\\022\000\023\000\035\000\033\000\
\\019\000\019\000\019\000\019\000\
\\019\000\019\000\019\000\019\000\
\\019\000\019\000\019\000\019\000\
\\019\000\060\000\061\000\020\000\
\\006\000\007\000\029\000\028\000\
\\058\000\057\000\056\000\054\000\
\\053\000\049\000\051\000\048\000\
\\050\000\046\000\045\000\043\000\
\\041\000\067\000\034\000\034\000\
\\010\000\011\000\018\000\015\000\
\\039\000\008\000\034\000\012\000\
\\016\000\038\000\000\000"
val gotoT =
"\
\\001\000\094\000\000\000\
\\007\000\002\000\000\000\
\\000\000\
\\000\000\
\\002\000\006\000\003\000\005\000\000\000\
\\002\000\008\000\003\000\005\000\000\000\
\\000\000\
\\006\000\011\000\007\000\010\000\000\000\
\\000\000\
\\004\000\014\000\005\000\013\000\007\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\033\000\008\000\032\000\009\000\031\000\010\000\030\000\
\\011\000\029\000\012\000\028\000\013\000\027\000\014\000\026\000\
\\015\000\025\000\016\000\024\000\000\000\
\\007\000\040\000\000\000\
\\007\000\033\000\008\000\041\000\009\000\031\000\010\000\030\000\
\\011\000\029\000\012\000\028\000\013\000\027\000\014\000\026\000\
\\015\000\025\000\016\000\024\000\000\000\
\\007\000\033\000\008\000\042\000\009\000\031\000\010\000\030\000\
\\011\000\029\000\012\000\028\000\013\000\027\000\014\000\026\000\
\\015\000\025\000\016\000\024\000\000\000\
\\006\000\043\000\007\000\010\000\000\000\
\\000\000\
\\007\000\033\000\008\000\046\000\009\000\031\000\010\000\030\000\
\\011\000\029\000\012\000\028\000\013\000\027\000\014\000\026\000\
\\015\000\025\000\016\000\024\000\000\000\
\\004\000\047\000\005\000\013\000\007\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\033\000\015\000\061\000\016\000\024\000\000\000\
\\000\000\
\\007\000\033\000\015\000\062\000\016\000\024\000\000\000\
\\000\000\
\\000\000\
\\007\000\033\000\008\000\063\000\009\000\031\000\010\000\030\000\
\\011\000\029\000\012\000\028\000\013\000\027\000\014\000\026\000\
\\015\000\025\000\016\000\024\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\033\000\015\000\068\000\016\000\024\000\000\000\
\\007\000\033\000\015\000\069\000\016\000\024\000\000\000\
\\007\000\033\000\015\000\070\000\016\000\024\000\000\000\
\\007\000\033\000\013\000\071\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\000\000\
\\007\000\033\000\013\000\072\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\000\000\
\\007\000\033\000\013\000\073\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\000\000\
\\007\000\033\000\013\000\074\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\000\000\
\\007\000\033\000\013\000\075\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\000\000\
\\007\000\033\000\013\000\076\000\014\000\026\000\015\000\025\000\
\\016\000\024\000\000\000\
\\007\000\033\000\012\000\077\000\013\000\027\000\014\000\026\000\
\\015\000\025\000\016\000\024\000\000\000\
\\007\000\033\000\012\000\078\000\013\000\027\000\014\000\026\000\
\\015\000\025\000\016\000\024\000\000\000\
\\007\000\033\000\011\000\079\000\012\000\028\000\013\000\027\000\
\\014\000\026\000\015\000\025\000\016\000\024\000\000\000\
\\007\000\033\000\010\000\080\000\011\000\029\000\012\000\028\000\
\\013\000\027\000\014\000\026\000\015\000\025\000\016\000\024\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\084\000\005\000\013\000\007\000\012\000\000\000\
\\004\000\085\000\005\000\013\000\007\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\091\000\005\000\013\000\007\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 95
val numrules = 44
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | NUMBER of  (int)
 | IDENTIFIER of  (string) | primary_exp of  (AST.EXP)
 | unary_exp of  (AST.EXP) | mult_exp of  (AST.EXP)
 | add_exp of  (AST.EXP) | rel_exp of  (AST.EXP)
 | eq_exp of  (AST.EXP) | and_exp of  (AST.EXP) | or_exp of  (AST.EXP)
 | expression of  (AST.EXP) | variable of  (string)
 | variablelist of  (string list) | command of  (AST.COMMAND)
 | commandlist of  (AST.COMMAND list)
 | declaration of  (AST.DECLARATION)
 | declarations of  (AST.DECLARATION list) | start of  (AST.PROGRAM)
end
type svalue = MlyValue.svalue
type result = AST.PROGRAM
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "PROGRAM"
  | (T 2) => "DCOLON"
  | (T 3) => "COLON"
  | (T 4) => "VAR"
  | (T 5) => "INT"
  | (T 6) => "BOOL"
  | (T 7) => "COMMA"
  | (T 8) => "LCPAREN"
  | (T 9) => "RCPAREN"
  | (T 10) => "ASSIGN"
  | (T 11) => "IF"
  | (T 12) => "THEN"
  | (T 13) => "ELSE"
  | (T 14) => "ENDIF"
  | (T 15) => "WHILE"
  | (T 16) => "DO"
  | (T 17) => "ENDWH"
  | (T 18) => "LPAREN"
  | (T 19) => "RPAREN"
  | (T 20) => "OR"
  | (T 21) => "AND"
  | (T 22) => "TRUE"
  | (T 23) => "FALSE"
  | (T 24) => "NOT"
  | (T 25) => "LT"
  | (T 26) => "GT"
  | (T 27) => "LTE"
  | (T 28) => "GTE"
  | (T 29) => "EQ"
  | (T 30) => "NEQ"
  | (T 31) => "PLUS"
  | (T 32) => "MINUS"
  | (T 33) => "MUL"
  | (T 34) => "DIV"
  | (T 35) => "MOD"
  | (T 36) => "SCOLON"
  | (T 37) => "READ"
  | (T 38) => "WRITE"
  | (T 39) => "IDENTIFIER"
  | (T 40) => "NUMBER"
  | (T 41) => "NEG"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33)
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, RCPAREN1right)) :: ( _, ( MlyValue.commandlist
 commandlist, _, _)) :: _ :: ( _, ( MlyValue.declarations declarations
, _, _)) :: _ :: ( _, ( MlyValue.variable variable, _, _)) :: ( _, ( _
, PROGRAM1left, _)) :: rest671)) => let val  result = MlyValue.start (
check_ident(variable); AST.PROG(variable, declarations, commandlist))
 in ( LrTable.NT 0, ( result, PROGRAM1left, RCPAREN1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.declarations declarations, _, 
declarations1right)) :: ( _, ( MlyValue.declaration declaration, 
declaration1left, _)) :: rest671)) => let val  result = 
MlyValue.declarations (declaration::declarations)
 in ( LrTable.NT 1, ( result, declaration1left, declarations1right), 
rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.declarations ([])
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( _, _, SCOLON1right)) :: _ :: _ :: ( _, ( 
MlyValue.variablelist variablelist, _, _)) :: ( _, ( _, VAR1left, _))
 :: rest671)) => let val  result = MlyValue.declaration (
 (insert_list(variablelist, 1)); AST.DECL(variablelist, AST.INT))
 in ( LrTable.NT 2, ( result, VAR1left, SCOLON1right), rest671)
end
|  ( 4, ( ( _, ( _, _, SCOLON1right)) :: _ :: _ :: ( _, ( 
MlyValue.variablelist variablelist, _, _)) :: ( _, ( _, VAR1left, _))
 :: rest671)) => let val  result = MlyValue.declaration (
 (insert_list(variablelist, 0)); AST.DECL(variablelist, AST.BOOL))
 in ( LrTable.NT 2, ( result, VAR1left, SCOLON1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.variablelist variablelist, _, 
variablelist1right)) :: _ :: ( _, ( MlyValue.variable variable, 
variable1left, _)) :: rest671)) => let val  result = 
MlyValue.variablelist (variable::variablelist)
 in ( LrTable.NT 5, ( result, variable1left, variablelist1right), 
rest671)
end
|  ( 6, ( ( _, ( MlyValue.variable variable, variable1left, 
variable1right)) :: rest671)) => let val  result = 
MlyValue.variablelist ([variable])
 in ( LrTable.NT 5, ( result, variable1left, variable1right), rest671)

end
|  ( 7, ( rest671)) => let val  result = MlyValue.variablelist ([])
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 8, ( ( _, ( MlyValue.commandlist commandlist, _, 
commandlist1right)) :: _ :: ( _, ( MlyValue.command command, 
command1left, _)) :: rest671)) => let val  result = 
MlyValue.commandlist (command::commandlist)
 in ( LrTable.NT 3, ( result, command1left, commandlist1right), 
rest671)
end
|  ( 9, ( rest671)) => let val  result = MlyValue.commandlist ([])
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 10, ( ( _, ( MlyValue.expression expression, _, expression1right)
) :: ( _, ( _, ASSIGNleft, _)) :: ( _, ( MlyValue.variable variable, 
variable1left, _)) :: rest671)) => let val  result = MlyValue.command
 (
if  ( check(variable) = 1 andalso check_iexp(expression) )
				orelse 
				( check(variable) = 0 andalso check_bexp(expression) )  then AST.SET(variable, expression) else(exit_prog ASSIGNleft; AST.ERROR2) 
)
 in ( LrTable.NT 4, ( result, variable1left, expression1right), 
rest671)
end
|  ( 11, ( ( _, ( MlyValue.variable variable, _, variable1right)) :: (
 _, ( _, (READleft as READ1left), _)) :: rest671)) => let val  result
 = MlyValue.command (
if ( check(variable)=1 orelse check(variable)=0 ) then AST.READ(variable) else(exit_prog READleft; AST.ERROR2)
)
 in ( LrTable.NT 4, ( result, READ1left, variable1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.expression expression, _, expression1right)
) :: ( _, ( _, (WRITEleft as WRITE1left), _)) :: rest671)) => let val 
 result = MlyValue.command (
if check_iexp(expression) then AST.WRITE(expression) else(exit_prog WRITEleft; AST.ERROR2) 
)
 in ( LrTable.NT 4, ( result, WRITE1left, expression1right), rest671)

end
|  ( 13, ( ( _, ( _, _, ENDIF1right)) :: _ :: ( _, ( 
MlyValue.commandlist commandlist2, _, _)) :: _ :: _ :: _ :: ( _, ( 
MlyValue.commandlist commandlist1, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.expression expression, _, _)) :: ( _, ( _, (IFleft as IF1left
), _)) :: rest671)) => let val  result = MlyValue.command (
if check_bexp(expression) then AST.ITE(expression, commandlist1, commandlist2) else(exit_prog IFleft; AST.ERROR2)
)
 in ( LrTable.NT 4, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 14, ( ( _, ( _, _, ENDWH1right)) :: _ :: ( _, ( 
MlyValue.commandlist commandlist, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.expression expression, _, _)) :: ( _, ( _, (WHILEleft as 
WHILE1left), _)) :: rest671)) => let val  result = MlyValue.command (
if check_bexp(expression) then AST.WHILE(expression, commandlist) else(exit_prog WHILEleft; AST.ERROR2)
)
 in ( LrTable.NT 4, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.or_exp or_exp, or_exp1left, or_exp1right))
 :: rest671)) => let val  result = MlyValue.expression (or_exp)
 in ( LrTable.NT 7, ( result, or_exp1left, or_exp1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.and_exp and_exp, _, and_exp1right)) :: ( _,
 ( _, ORleft, _)) :: ( _, ( MlyValue.or_exp or_exp, or_exp1left, _))
 :: rest671)) => let val  result = MlyValue.or_exp (
if check_bexp2(or_exp, and_exp) then AST.OR(or_exp, and_exp) else (exit_prog ORleft; AST.ERROR)
)
 in ( LrTable.NT 8, ( result, or_exp1left, and_exp1right), rest671)

end
|  ( 17, ( ( _, ( MlyValue.and_exp and_exp, and_exp1left, 
and_exp1right)) :: rest671)) => let val  result = MlyValue.or_exp (
and_exp)
 in ( LrTable.NT 8, ( result, and_exp1left, and_exp1right), rest671)

end
|  ( 18, ( ( _, ( MlyValue.eq_exp eq_exp, _, eq_exp1right)) :: ( _, (
 _, ANDleft, _)) :: ( _, ( MlyValue.and_exp and_exp, and_exp1left, _))
 :: rest671)) => let val  result = MlyValue.and_exp (
if check_bexp2(and_exp, eq_exp) then AST.AND(and_exp, eq_exp) else (exit_prog ANDleft; AST.ERROR)
)
 in ( LrTable.NT 9, ( result, and_exp1left, eq_exp1right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.eq_exp eq_exp, eq_exp1left, eq_exp1right))
 :: rest671)) => let val  result = MlyValue.and_exp (eq_exp)
 in ( LrTable.NT 9, ( result, eq_exp1left, eq_exp1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.rel_exp rel_exp, _, rel_exp1right)) :: ( _,
 ( _, EQleft, _)) :: ( _, ( MlyValue.eq_exp eq_exp, eq_exp1left, _))
 :: rest671)) => let val  result = MlyValue.eq_exp (
if check_i2_b2(eq_exp, rel_exp) then AST.EQ(eq_exp, rel_exp) else (exit_prog EQleft; AST.ERROR)
)
 in ( LrTable.NT 10, ( result, eq_exp1left, rel_exp1right), rest671)

end
|  ( 21, ( ( _, ( MlyValue.rel_exp rel_exp, _, rel_exp1right)) :: ( _,
 ( _, NEQleft, _)) :: ( _, ( MlyValue.eq_exp eq_exp, eq_exp1left, _))
 :: rest671)) => let val  result = MlyValue.eq_exp (
if check_i2_b2(eq_exp, rel_exp) then AST.NEQ(eq_exp, rel_exp) else (exit_prog NEQleft; AST.ERROR)
)
 in ( LrTable.NT 10, ( result, eq_exp1left, rel_exp1right), rest671)

end
|  ( 22, ( ( _, ( MlyValue.rel_exp rel_exp, rel_exp1left, 
rel_exp1right)) :: rest671)) => let val  result = MlyValue.eq_exp (
rel_exp)
 in ( LrTable.NT 10, ( result, rel_exp1left, rel_exp1right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.add_exp add_exp, _, add_exp1right)) :: ( _,
 ( _, GTleft, _)) :: ( _, ( MlyValue.rel_exp rel_exp, rel_exp1left, _)
) :: rest671)) => let val  result = MlyValue.rel_exp (
if check_i2_b2(rel_exp, add_exp) then AST.GT(rel_exp, add_exp) else (exit_prog GTleft; AST.ERROR)
)
 in ( LrTable.NT 11, ( result, rel_exp1left, add_exp1right), rest671)

end
|  ( 24, ( ( _, ( MlyValue.add_exp add_exp, _, add_exp1right)) :: ( _,
 ( _, GTEleft, _)) :: ( _, ( MlyValue.rel_exp rel_exp, rel_exp1left, _
)) :: rest671)) => let val  result = MlyValue.rel_exp (
if check_i2_b2(rel_exp, add_exp) then AST.GTE(rel_exp, add_exp) else (exit_prog GTEleft; AST.ERROR)
)
 in ( LrTable.NT 11, ( result, rel_exp1left, add_exp1right), rest671)

end
|  ( 25, ( ( _, ( MlyValue.add_exp add_exp, _, add_exp1right)) :: ( _,
 ( _, LTleft, _)) :: ( _, ( MlyValue.rel_exp rel_exp, rel_exp1left, _)
) :: rest671)) => let val  result = MlyValue.rel_exp (
if check_i2_b2(rel_exp, add_exp) then AST.LT(rel_exp, add_exp) else (exit_prog LTleft; AST.ERROR)
)
 in ( LrTable.NT 11, ( result, rel_exp1left, add_exp1right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.add_exp add_exp, _, add_exp1right)) :: ( _,
 ( _, LTEleft, _)) :: ( _, ( MlyValue.rel_exp rel_exp, rel_exp1left, _
)) :: rest671)) => let val  result = MlyValue.rel_exp (
if check_i2_b2(rel_exp, add_exp) then AST.LTE(rel_exp, add_exp) else (exit_prog LTEleft; AST.ERROR)
)
 in ( LrTable.NT 11, ( result, rel_exp1left, add_exp1right), rest671)

end
|  ( 27, ( ( _, ( MlyValue.add_exp add_exp, add_exp1left, 
add_exp1right)) :: rest671)) => let val  result = MlyValue.rel_exp (
add_exp)
 in ( LrTable.NT 11, ( result, add_exp1left, add_exp1right), rest671)

end
|  ( 28, ( ( _, ( MlyValue.add_exp add_exp, _, add_exp1right)) :: ( _,
 ( _, PLUSleft, _)) :: ( _, ( MlyValue.mult_exp mult_exp, 
mult_exp1left, _)) :: rest671)) => let val  result = MlyValue.add_exp
 (
if check_iexp2(mult_exp, add_exp)=true then AST.PLUS(mult_exp, add_exp) else (exit_prog PLUSleft; AST.ERROR)
)
 in ( LrTable.NT 12, ( result, mult_exp1left, add_exp1right), rest671)

end
|  ( 29, ( ( _, ( MlyValue.add_exp add_exp, _, add_exp1right)) :: ( _,
 ( _, MINUSleft, _)) :: ( _, ( MlyValue.mult_exp mult_exp, 
mult_exp1left, _)) :: rest671)) => let val  result = MlyValue.add_exp
 (
if check_iexp2(mult_exp, add_exp)=true then AST.MINUS(mult_exp, add_exp) else (exit_prog MINUSleft; AST.ERROR)
)
 in ( LrTable.NT 12, ( result, mult_exp1left, add_exp1right), rest671)

end
|  ( 30, ( ( _, ( MlyValue.mult_exp mult_exp, mult_exp1left, 
mult_exp1right)) :: rest671)) => let val  result = MlyValue.add_exp (
mult_exp)
 in ( LrTable.NT 12, ( result, mult_exp1left, mult_exp1right), rest671
)
end
|  ( 31, ( ( _, ( MlyValue.unary_exp unary_exp, _, unary_exp1right))
 :: ( _, ( _, MULleft, _)) :: ( _, ( MlyValue.mult_exp mult_exp, 
mult_exp1left, _)) :: rest671)) => let val  result = MlyValue.mult_exp
 (
if check_iexp2(mult_exp, unary_exp)=true  then AST.TIMES(mult_exp, unary_exp) else (exit_prog MULleft; AST.ERROR)
)
 in ( LrTable.NT 13, ( result, mult_exp1left, unary_exp1right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.unary_exp unary_exp, _, unary_exp1right))
 :: ( _, ( _, DIVleft, _)) :: ( _, ( MlyValue.mult_exp mult_exp, 
mult_exp1left, _)) :: rest671)) => let val  result = MlyValue.mult_exp
 (
if check_iexp2(mult_exp, unary_exp)=true  then AST.DIV(mult_exp, unary_exp) else (exit_prog DIVleft; AST.ERROR)
)
 in ( LrTable.NT 13, ( result, mult_exp1left, unary_exp1right), 
rest671)
end
|  ( 33, ( ( _, ( MlyValue.unary_exp unary_exp, _, unary_exp1right))
 :: ( _, ( _, MODleft, _)) :: ( _, ( MlyValue.mult_exp mult_exp, 
mult_exp1left, _)) :: rest671)) => let val  result = MlyValue.mult_exp
 (
if check_iexp2(mult_exp, unary_exp)=true  then AST.MOD(mult_exp, unary_exp) else (exit_prog MODleft; AST.ERROR)
)
 in ( LrTable.NT 13, ( result, mult_exp1left, unary_exp1right), 
rest671)
end
|  ( 34, ( ( _, ( MlyValue.unary_exp unary_exp, unary_exp1left, 
unary_exp1right)) :: rest671)) => let val  result = MlyValue.mult_exp
 (unary_exp)
 in ( LrTable.NT 13, ( result, unary_exp1left, unary_exp1right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.unary_exp unary_exp, _, unary_exp1right))
 :: ( _, ( _, (NEGleft as NEG1left), _)) :: rest671)) => let val  
result = MlyValue.unary_exp (
if check_iexp(unary_exp)=true then (AST.NEG(unary_exp)) else (exit_prog NEGleft; AST.ERROR)
)
 in ( LrTable.NT 14, ( result, NEG1left, unary_exp1right), rest671)

end
|  ( 36, ( ( _, ( MlyValue.unary_exp unary_exp, _, unary_exp1right))
 :: ( _, ( _, (NOTleft as NOT1left), _)) :: rest671)) => let val  
result = MlyValue.unary_exp (
if check_bexp(unary_exp)=true then (AST.NOT(unary_exp)) else (exit_prog NOTleft; AST.ERROR)
)
 in ( LrTable.NT 14, ( result, NOT1left, unary_exp1right), rest671)

end
|  ( 37, ( ( _, ( MlyValue.primary_exp primary_exp, primary_exp1left, 
primary_exp1right)) :: rest671)) => let val  result = 
MlyValue.unary_exp (primary_exp)
 in ( LrTable.NT 14, ( result, primary_exp1left, primary_exp1right), 
rest671)
end
|  ( 38, ( ( _, ( MlyValue.variable variable, variable1left, 
variable1right)) :: rest671)) => let val  result = 
MlyValue.primary_exp (
if check (variable) = 1 then AST.ID(variable) else if check(variable) = 0 then AST.IDB(variable) else (exit_prog 2; AST.ID(variable))
)
 in ( LrTable.NT 15, ( result, variable1left, variable1right), rest671
)
end
|  ( 39, ( ( _, ( MlyValue.NUMBER NUMBER, NUMBER1left, NUMBER1right))
 :: rest671)) => let val  result = MlyValue.primary_exp (
AST.INTEGER(NUMBER))
 in ( LrTable.NT 15, ( result, NUMBER1left, NUMBER1right), rest671)

end
|  ( 40, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.primary_exp (AST.TT)
 in ( LrTable.NT 15, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 41, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.primary_exp (AST.FF)
 in ( LrTable.NT 15, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 42, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.primary_exp (expression)
 in ( LrTable.NT 15, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 43, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.variable
 (check_ident(IDENTIFIER); IDENTIFIER)
 in ( LrTable.NT 6, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : C_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun PROGRAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun DCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun LCPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun RCPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun LTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun GTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun SCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.IDENTIFIER i,p1,p2))
fun NUMBER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.NUMBER i,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
