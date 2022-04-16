1. How to run:

sml while_ast.sml test

where test is the WHILE program test file 

2. Signature of VMC:

signature VMC =
sig
  exception VMCERROR
  val mem_size : int
  val garbage : int
  val memory : int array
  val index : int ref
  val printList : string list -> string
  structure StrHashmap :
    sig
      structure Key : <sig>
      datatype 'a hash_table = ...
      val mkTable : int * exn -> 'a hash_table
      val clear : 'a hash_table -> unit
      val insert : 'a hash_table -> Key.hash_key * 'a -> unit
      val inDomain : 'a hash_table -> Key.hash_key -> bool
      val lookup : 'a hash_table -> Key.hash_key -> 'a
      val find : 'a hash_table -> Key.hash_key -> 'a option
      val remove : 'a hash_table -> Key.hash_key -> 'a
      val numItems : 'a hash_table -> int
      val listItems : 'a hash_table -> 'a list
      val listItemsi : 'a hash_table -> (Key.hash_key * 'a) list
      val app : ('a -> unit) -> 'a hash_table -> unit
      val appi : (Key.hash_key * 'a -> unit) -> 'a hash_table -> unit
      val map : ('a -> 'b) -> 'a hash_table -> 'b hash_table
      val mapi : (Key.hash_key * 'a -> 'b) -> 'a hash_table -> 'b hash_table
      val fold : ('a * 'b -> 'b) -> 'b -> 'a hash_table -> 'b
      val foldi : (Key.hash_key * 'a * 'b -> 'b) -> 'b -> 'a hash_table -> 'b
      val modify : ('a -> 'a) -> 'a hash_table -> unit
      val modifyi : (Key.hash_key * 'a -> 'a) -> 'a hash_table -> unit
      val filter : ('a -> bool) -> 'a hash_table -> unit
      val filteri : (Key.hash_key * 'a -> bool) -> 'a hash_table -> unit
      val copy : 'a hash_table -> 'a hash_table
      val bucketSizes : 'a hash_table -> int list
    end
  val symTbl : int Vmc.StrHashmap.hash_table
  val var_int_list : string list ref
  val var_bool_list : string list ref
  val err_msg : string -> unit
  val check : string -> int
  val insert_list : string list * int -> bool
  val fill_helper : AST.DECLARATION -> bool
  val fill_list : AST.DECLARATION list -> bool
  val fill_symbol_table : AST.PROGRAM -> bool
  val string_var_list : int array * string list -> string
  val Id : 'a -> 'a
  val toString : string FunStack.Stack * int array * string FunStack.Stack
                 -> unit
  val Vstack : 'a FunStack.Stack
  val Cstack : 'a FunStack.Stack
  val delete_ith : 'a list * int -> 'a list
  val eval : int * int * string -> int
  val eval_1op : int * string -> int
  val get_comm_list1 : string FunStack.Stack * int
                       -> string FunStack.Stack * string list
  val get_comm_list2 : string FunStack.Stack * int
                       -> string FunStack.Stack * string list
  val get_comm_list3 : string FunStack.Stack * int
                       -> string FunStack.Stack * string list
  val get_comm_list4 : string FunStack.Stack * int
                       -> string FunStack.Stack * string list
  val push_list_on_stack : 'a FunStack.Stack * 'a list -> 'a FunStack.Stack
  val get_input : string -> string
  val rules : string FunStack.Stack * int array * string FunStack.Stack
              -> string FunStack.Stack * int array * string FunStack.Stack

end


3. File description:

    i. while_ast.sml - Driver file 
        - calls parser.sml functions to convert the program to AST, 
        - calls postfix from AST.sml to convert AST to postfix string
        - initializes symbol table, control stack and calls "execute" to execute the program

    ii. stack.sml - contains structure for Funstack, a stack implementation with the exact same
        signature given in assignment pdf. Stack made using list module of SML

    iii. vmc.sml - contains Vmc structure that implements "rules" function. 
            - Creates a symbol table for memory indexing and distinguishig between bool and int variables
            - many helper functions like inserting to symbol table, inserting to stack
            - creates the main memory with fixed size and inialized with a garbage value
            - creates the two stack V and C and initializes C with program commands

    iv. AST.sml - contains structure for AST and postfix function to convert AST to postfix.
        - postfix expression is created using postorder tarversal of the program AST

    v. parse.sml - contains structure for parser for coordinating lexer and yacc to generate AST.
    vi. prog.grm - yacc file containing grammar and symbol table for type checking
    vii. prog.lex - lex file for lexical analysis. Points out the error in line number and column number
    

Design Choices:

1. Stack is implemented using List module of SML
2. postfix string is created by doign a postorder traversal of program AST.
3. Executing the ITE and WH commands are tricky as they involve infinite lookahead. We need to know 
    exactly when to begin, when to start and the separation between different blocks 
    (like then and else block in if). 
    For facilitating that, I have inserted special keywords in the postfix expression of the AST.


    FOR EVALUATING ITE - Assume the three blocks in ITE are bexp(if bool exp), comm1 (then block) 
    and comm2 (else block). What I do here is that I insert the keyword "if" after postfix expression of
    bexp, "then" after postfix expression of comm1 and "else" after postfix expression of comm2.

    Then in the control stack when I reach "if", it means the bexp would have already been evaluated
    and will be on top of V. I pop it and store it as "b_val".
    The I use the stack V and C to pop out comm1 if b_val = 1, else I pop out comm2. 

    Now there is the issue of how to determine exact block of comm1 and comm2, this is where the 
    keywords "then" and "else" come useful. When extracting the comm1 block (using function "get_comm_list1")
    I am looking for "then" keyword in stack C, as it will indicate the end of comm1 block. As simple as that!
    
    However, in the case of nested ifs, there can be more than 1 "then"s. So I also keep a counter = 0.
    While looking ahead for comm1 block, if I encounter an "if" keyword, I increment the counter by 1.
    If a "then" keyword is encountered, the counter is decremented by 1. It is only when I encounter a "then"
    keyword AND my counter = 0, I will find the entire comm1 block.

    To understand better, Think of it as a nested brackets situation. "if" is like starting bracket, 
    "then" is like ending bracket. So a nested "if then else" may look like {{}} or {{}{}{}}. In any case,
    if we maintain a counter and increment it at starting bracket and decrement at clsing bracket, we will
    get 0 at the end of balanced paranthesis. That is the concept I have used to extract command or expression blocks

    NOTE: As these keywords are already reerved keywords, there is no chance of them being identifirs and 
    hence there will be no issue during their mixup with variable names.


    FOR EVALUATING WH - I use keywords "while" at the very start of entire block, "do" after the bexp
    and "endwh" after command block. 
    When I encounter "while", I extract bexp and comm blocks (same way as above using V and C stack and
    using the special keywords) and then foll the rule WH.? from assignment. I pop bexp and comm from C
    to and then push bexp on top to make it bexp."DO".C. And push bexp and comm to V to make it comm.bexp.V.

    When I encounter "do", it is when I have evaluted bexp and the resul b_val is on top of V stack.
    I pop and store it as b_val. I implement logic of WH.0 and WH.1. i decide it based on b_val.
    First I extract comm and bexp from V and then decide as the rules of assignment pdf. This is pretty similar
    to above in implementation. 

4. For read - I am using SML commands to interrupt the program and take input using a prompt
5. For write- I am writing to terminal.

6. Error handling - Undeclared identifiers throw an erorr with appropriate message. Also exits the program.
 - When "read x" is a command and x is bool, I check if the user input is 0 or 1 and if not then throw BOOLerror.
 - When reading, if an inappropriate value (like string instead of integer) is entered, INTerror is thrown
 - When duplicate variables are declared, DUPVARerror is thrown.

7. Efficiency - Symbol table is implemented as a hashmap. SO lookup are O(1) average.

8. When printing the memory, I use a variables list to print only the declared variables and not the entire memory.
As printing enitre memory is not useful as there are many useless entries we are not interested in.

9. For assigning variable to an index in memory, i am using a reference integer which i am incrementing
after allocating memory to rach variable.

10. Vmc.toString(V,M,C) prints all three in a nice intutive format.

11. All variables are initialized to 0 as stated in asssignment.

12. The size of the memory can adjusted easily at start of the run, by changing the variable "mem_size" in vmc.sml
