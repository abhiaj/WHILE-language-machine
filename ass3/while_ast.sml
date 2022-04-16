val argv = CommandLine.arguments();

Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
CM.make("sources.cm");
use "AST.sml";
use "prog.grm.sig";
use "prog.grm.sml";
use "prog.lex.sml";
use "parse.sml";

(* fun printToFile(string_to_print, filename) = 
    let val file = TextIO.openOut filename
    in (TextIO.output (file, string_to_print); TextIO.closeOut file)
    end *)


val _ = TextIO.output(TextIO.stdOut, "\n\n\n######\n\n\n");

val parseOutput = Parser.parse (List.hd argv);

val _ = TextIO.output(TextIO.stdOut, "\n\n\n######\n\n");


OS.Process.exit(OS.Process.success);