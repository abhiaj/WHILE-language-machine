structure Parser =
struct

    structure CLrVals = CLrValsFun(structure Token = LrParser.Token);
    structure CLex    = CLexFun(structure Tokens = CLrVals.Tokens);
    structure CParser = Join(
                                structure ParserData = CLrVals.ParserData
                                structure Lex = CLex
                                structure LrParser = LrParser);
    
    exception CError;
    fun parse fileName = 
        let val inStream = TextIO.openIn fileName
	    fun readNext n = if TextIO.endOfStream inStream then ""
	                     else TextIO.inputN (inStream, n)
            val lexer = CParser.makeLexer readNext
            fun printError (msg,line,col) = print (fileName ^ "[" ^ Int.toString line ^ "] " ^msg^ "\n")
	    val (ans,_) = CParser.parse (15, lexer, printError, ())
	in
            ans
	end
end




