exception ScanError

structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  

  type lexresult = (svalue, pos) token

  val pos = ref 1
  val col = ref 1
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, l, ll) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ "." ^ (Int.toString ll) ^ ": " ^ e ^ "\n")
  
  val increase_col = fn (e, col) => let val temp = !col in (col := !col + size(e); temp) end

  
%%
%header (functor CLexFun(structure Tokens: C_TOKENS));

alphanumeric=[a-zA-Z0-9];
alphanumspace=[a-z\-A-Z0-9\ \n\t\r\!\#@\$\^\&\*\{\}\<\>\%\+\#_];
quotes = "\"";
number = [0-9];
alpha = [a-zA-Z];
ws = [\ \t\r];
letter = [a-zA-Z];
digit = [0-9]+;

id = {letter}({letter}|{digit})*;

%%
\n                                => ( pos := (!pos) + 1; col := 1; lex() );

"program" => ( Tokens.PROGRAM(!pos, increase_col(yytext, col)) );
"::" => ( Tokens.DCOLON(!pos, increase_col(yytext, col)) );
":" => ( Tokens.COLON(!pos, increase_col(yytext, col)) );
"var" => ( Tokens.VAR(!pos, increase_col(yytext, col)) );
"int" => ( Tokens.INT(!pos, increase_col(yytext, col)) );
"bool" => ( Tokens.BOOL(!pos, increase_col(yytext, col)) );
"," => ( Tokens.COMMA(!pos, increase_col(yytext, col)) );
"{" => ( Tokens.LCPAREN(!pos, increase_col(yytext, col)) );
"}" => ( Tokens.RCPAREN(!pos, increase_col(yytext, col)) );
":=" => ( Tokens.ASSIGN(!pos, increase_col(yytext, col)) );
"if" => ( Tokens.IF(!pos, increase_col(yytext, col)) );
"then" => ( Tokens.THEN(!pos, increase_col(yytext, col)) );
"else" => ( Tokens.ELSE(!pos, increase_col(yytext, col)) );
"endif" => ( Tokens.ENDIF(!pos, increase_col(yytext, col)) );
"while" => ( Tokens.WHILE(!pos, increase_col(yytext, col)) );
"do" => ( Tokens.DO(!pos, increase_col(yytext, col)) );
"endwh" => ( Tokens.ENDWH(!pos, increase_col(yytext, col)) );
"(" => ( Tokens.LPAREN(!pos, increase_col(yytext, col)) );
")" => ( Tokens.RPAREN(!pos, increase_col(yytext, col)) );
"||" => ( Tokens.OR(!pos, increase_col(yytext, col)) );
"&&" => ( Tokens.AND(!pos, increase_col(yytext, col)) );
"tt" => ( Tokens.TRUE(!pos, increase_col(yytext, col)) );
"ff" => ( Tokens.FALSE(!pos, increase_col(yytext, col)) );

"!" => ( Tokens.NOT(!pos, increase_col(yytext, col)) );

"<" => ( Tokens.LT(!pos, increase_col(yytext, col)) );
">" => ( Tokens.GT(!pos, increase_col(yytext, col)) );
"<=" => ( Tokens.LTE(!pos, increase_col(yytext, col)) );
">=" => ( Tokens.GTE(!pos, increase_col(yytext, col)) );
"=" => ( Tokens.EQ(!pos, increase_col(yytext, col)) );
"<>" => ( Tokens.NEQ(!pos, increase_col(yytext, col)) );

"+" => ( Tokens.PLUS(!pos, increase_col(yytext, col)) );
"-" => ( Tokens.MINUS(!pos, increase_col(yytext, col)) );

"*" => ( Tokens.MUL(!pos, increase_col(yytext, col)) );
"/" => ( Tokens.DIV(!pos, increase_col(yytext, col)) );
"%" => ( Tokens.MOD(!pos, increase_col(yytext, col)) );

"~" => ( Tokens.NEG(!pos, increase_col(yytext, col)) );

";"  => ( Tokens.SCOLON(!pos, increase_col(yytext, col)) );

"read"  => ( Tokens.READ(!pos, increase_col(yytext, col)) );
"write"  => ( Tokens.WRITE(!pos, increase_col(yytext, col)) );


{number}+ => ( Tokens.NUMBER( valOf(Int.fromString yytext), !pos, increase_col(yytext, col) ) );
{id} => ( Tokens.IDENTIFIER(yytext, !pos, increase_col(yytext, col)) );

{ws}+                             => ( increase_col(yytext, col) ;lex() );
.           => (error ("Raisisng ScanError  "^yytext,!pos,!col); increase_col(yytext, col); raise ScanError; lex());
