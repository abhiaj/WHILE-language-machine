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
use "vmc.sml";

open Vmc;

(* fun printToFile(string_to_print, filename) = 
    let val file = TextIO.openOut filename
    in (TextIO.output (file, string_to_print); TextIO.closeOut file)
    end *)

fun execute (V, M, C) = case (FunStack.empty(C)) of
        true => (V, M, C)

        | false =>
            let
                val (Vn, Mn, Cn) = rules(V,M,C)
                (* val _ = toString(Vn, Mn, Cn) *)
            in execute(Vn, Mn, Cn)
            end

val _ = TextIO.output(TextIO.stdOut, "\n\n\n######\n\n\n");

val parseOutput = Parser.parse (List.hd argv);

val str_postfix = AST.postfix (parseOutput);
val spaceSep = String.tokens (fn c => c = #" ");
val str_postfix_list = spaceSep str_postfix;

val Cstack = push_list_on_stack(Cstack, str_postfix_list);

val _ = fill_symbol_table(parseOutput);

val (Vn, Mn, Cn) = execute(Vstack, memory, Cstack);
val _ = toString(Vn, Mn, Cn);

val _ = TextIO.output(TextIO.stdOut, "\n\n\n######\n\n");


OS.Process.exit(OS.Process.success);