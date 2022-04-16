use "stack.sml";
 (* use "AST.sml"; *)


(* signature VMC =
  sig
    exception VMCERROR
    val mem_size : int
    val garbage : int
    val memory : int array
    val index : int ref
    structure StrHashmap : <sig>
    val symTbl : int StrHashmap.hash_table
    val err_msg : string -> unit
    val check : string -> int
    val insert_list : string list * 'a -> bool
    val fill_helper : AST.DECLARATION -> bool
    val fill_list : AST.DECLARATION list -> bool
    val fill_symbol_table : AST.PROGRAM -> bool
    val Id : 'a -> 'a
    val toString : string FunStack.Stack * 'a * string FunStack.Stack -> unit
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
  end *)

structure Vmc  =
struct

    exception VMCERROR;
    exception BOOLERROR;
    exception DUPVARERROR;

    val mem_size = 200
    val garbage = 0
    val memory = Array.array(mem_size, garbage)

    val index = ref 0

    fun printList [] = ""
    | printList (x::tail) = x^"\n"^printList(tail)

    fun exist (_, []) = false
    | exist (x, y::ys) = x = y orelse exist (x, ys)

    fun delete_ith ([], k) = []
    | delete_ith (x::xs, 1) = xs
    | delete_ith (x::xs, k) = x :: delete_ith (xs, k - 1)

    

    (*Symbol table / Hashmap for storing the list of declared variables *)
    structure StrHashmap = HashTableFn(
    struct
        type hash_key = string
        val hashVal = HashString.hashString
        val sameKey = (op=) : string * string -> bool
    end)

    val symTbl : int StrHashmap.hash_table = StrHashmap.mkTable(mem_size, Fail "Not found")

    val var_int_list = ref [] : string list ref
    val var_bool_list = ref [] : string list ref

    (* check function to check if string is in hashmap or not
    if it is in hashmap, it retuns its index in memory *)
    val err_msg = fn variable => TextIO.output(TextIO.stdOut, "PARSE error\n"^"variable "^variable^" not defined\n\n\n")
    val check = fn variable => case ( StrHashmap.find symTbl variable ) of 
                    SOME idx => idx
                    | _ => (err_msg(variable); OS.Process.exit(OS.Process.failure); 2)


    (* var_type, 1 is int, 0 is bool *)
    (* function to insert list of variables from declaration to hashmap *)
    fun insert_list (var::list, 1) = ( StrHashmap.insert symTbl (var, (!index)); 
            if (exist (var, !var_int_list) = true) then raise DUPVARERROR else();
            if (exist (var, !var_bool_list) = true) then raise DUPVARERROR else();
            var_int_list := var:: (!var_int_list);
            index:= (!index)+1; insert_list (list, 1) )
        | insert_list (var::list, 0) = ( StrHashmap.insert symTbl (var, (!index)); 
            if (exist (var, !var_int_list) = true) then raise DUPVARERROR else();
            if (exist (var, !var_bool_list) = true) then raise DUPVARERROR else();
            var_bool_list := var:: (!var_bool_list);
            index:= (!index)+1; insert_list (list, 0) )

        | insert_list ([], var_type) = true

    fun fill_helper (AST.DECL(strL, AST.INT)) = insert_list(strL, 1)
        | fill_helper (AST.DECL(strL, AST.BOOL)) = insert_list(strL, 0)

    fun fill_list []         = true
        |   fill_list (x::xs)    =  fill_helper(x) andalso fill_list(xs);

    fun fill_symbol_table(AST.PROG(_, DecL, CommL)) = fill_list(DecL)


    fun string_var_list (M, []) = ""
    | string_var_list (M, x::L) = 
            let 
                val idx = check(x)
                val x_val = Int.toString(Array.sub(M, idx)) 
                
            in x^": "^x_val^", "^string_var_list(M, L)
            end

    fun Id (x) = x
    fun toString(V, M, C) = TextIO.output(TextIO.stdOut, "\n\nV: "^(FunStack.toString Id V) ^"\nM: "^ (string_var_list(M, !var_int_list)) ^"\n\t"^ (string_var_list(M, !var_bool_list))^"\nC: "^ (FunStack.toString Id C)^"\n\n")



    val Vstack = FunStack.create
    val Cstack = FunStack.create




    fun eval(m, n, opn) = case opn of
    "PLUS" => m+n
    | "MINUS" => m-n
    | "DIV" => m div n 
    | "MOD" => m mod n 
    | "TIMES" => m*n
    | "GT" => if(m > n) then 1 else 0 
    | "LT" => if(m < n) then 1 else 0  
    | "GTE" => if(m >= n) then 1 else 0  
    | "LTE" => if(m <= n) then 1 else 0  
    | "EQ" => if(m = n) then 1 else 0  
    | "NEQ" => if(m <> n) then 1 else 0 
    | "AND" => Int.min(m, n)
    | "OR" => Int.max(m, n)
    | _ => raise VMCERROR

    fun eval_1op(m, opn) = case opn of
    "NEG" => ~m
    | "NOT" => if m = 0 then 1 else 0
    | _ => raise VMCERROR

    fun get_comm_list1(C, num) = case ( FunStack.top(C) ) of
        "IF" => let
                    val num1 = num+1

                    val x = FunStack.top(C)
                    val C1 = FunStack.pop(C)

                    val (C2, L) = get_comm_list1(C1, num1)
                in  (C2, x::L)
                end

        | "THEN" => 
                let
                    val num1 = num-1

                    val x = FunStack.top(C)
                    val C1 = FunStack.pop(C)

                in
                    if (num1 = 0) 
                    then (C1, [])
                    else (let val (C2, L) = get_comm_list1(C1, num1) in (C2, x::L) end)
                end
        | _ =>
                let
                    val x = FunStack.top(C)
                    val C1 = FunStack.pop(C)

                    val (C2, L) = get_comm_list1(C1, num)
                in  (C2, x::L)
                end


    fun get_comm_list2(C, num) = case ( FunStack.top(C) ) of
        "IF" => let
                    val num1 = num+1

                    val x = FunStack.top(C)
                    val C1 = FunStack.pop(C)

                    val (C2, L) = get_comm_list2(C1, num1)
                in  (C2, x::L)
                end

        | "ELSE" => 
                let
                    val num1 = num-1

                    val x = FunStack.top(C)
                    val C1 = FunStack.pop(C)

                in
                    if (num1 = 0) 
                    then (C1, [])
                    else (let val (C2, L) = get_comm_list2(C1, num1) in (C2, x::L) end)
                end
        | _ =>
                let
                    val x = FunStack.top(C)
                    val C1 = FunStack.pop(C)

                    val (C2, L) = get_comm_list2(C1, num)
                in  (C2, x::L)
                end

    fun get_comm_list3(C, num) = case ( FunStack.top(C) ) of
        "WHILE" => let
                    val num1 = num+1

                    val x = FunStack.top(C)
                    val C1 = FunStack.pop(C)

                    val (C2, L) = get_comm_list3(C1, num1)
                in  (C2, x::L)
                end

        | "DO" => 
                let
                    val num1 = num-1

                    val x = FunStack.top(C)
                    val C1 = FunStack.pop(C)

                in
                    if (num1 = 0) 
                    then (C1, [x])
                    else (let val (C2, L) = get_comm_list3(C1, num1) in (C2, x::L) end)
                end
        | _ =>
                let
                    val x = FunStack.top(C)
                    val C1 = FunStack.pop(C)

                    val (C2, L) = get_comm_list3(C1, num)
                in  (C2, x::L)
                end

    fun get_comm_list4(C, num) = case ( FunStack.top(C) ) of
        "WHILE" => let
                    val num1 = num+1

                    val x = FunStack.top(C)
                    val C1 = FunStack.pop(C)

                    val (C2, L) = get_comm_list4(C1, num1)
                in  (C2, x::L)
                end

        | "ENDWH" => 
                let
                    val num1 = num-1

                    val x = FunStack.top(C)
                    val C1 = FunStack.pop(C)

                in
                    if (num1 = 0) 
                    then (C1, [x])
                    else (let val (C2, L) = get_comm_list4(C1, num1) in (C2, x::L) end)
                end
        | _ =>
                let
                    val x = FunStack.top(C)
                    val C1 = FunStack.pop(C)

                    val (C2, L) = get_comm_list4(C1, num)
                in  (C2, x::L)
                end

    fun push_list_on_stack (S, []) = S
        | push_list_on_stack (S, x::L) = 
                let 
                    val S1 = push_list_on_stack(S, L)
                    val S2 = FunStack.push(x, S1)
                in 
                    S2
                end

    fun get_input variable = ( TextIO.output(TextIO.stdOut, "Input "^variable^": ")
                 ; TextIO.flushOut(TextIO.stdOut)
                 ; valOf(TextIO.inputLine TextIO.stdIn)
                 )



    fun rules (V, M, C) = case (FunStack.top(C)) of 
        "ID" => let
                    val x = FunStack.top(V)
                    val V1 = FunStack.pop(V)

                    val idx = check(x)
                    val x_val = Int.toString(Array.sub(M, idx))
                    val V2 = FunStack.push(x_val, V1)

                    val C1 = FunStack.pop(C)
                in (V2, M, C1)
                end

        | "IDB" => 
                let
                    val x = FunStack.top(V)
                    val V1 = FunStack.pop(V)

                    val idx = check(x)
                    val x_val = Int.toString(Array.sub(M, idx))
                    val V2 = FunStack.push(x_val, V1)

                    val C1 = FunStack.pop(C)
                in (V2, M, C1)
                end

        | "TT" => 
                let
                    val V1 = FunStack.push("1", V)

                    val C1 = FunStack.pop(C)
                in (V1, M, C1)
                end

        | "FF" => 
                let
                    val V1 = FunStack.push("0", V)

                    val C1 = FunStack.pop(C)
                in (V1, M, C1)
                end

        | ( "PLUS" | "MINUS" | "DIV" | "MOD" | "TIMES" 
            | "GT" | "LT" | "GTE" | "LTE" | "EQ" | "NEQ" 
            | "AND" | "OR" ) => 
                let 
                    val n = FunStack.top(V)
                    val V1 = FunStack.pop(V)

                    val m = FunStack.top(V1)
                    val V2 = FunStack.pop(V1)

                    val opn = FunStack.top(C)
                    
                    val res = eval(valOf(Int.fromString(m)), valOf(Int.fromString(n)), opn)

                    val V3 = FunStack.push(Int.toString(res), V2)

                    val C1 = FunStack.pop(C)
                in (V3, M, C1)
                end

        | ("NOT" | "NEG") => 
                let
                    val m = FunStack.top(V)
                    val V1 = FunStack.pop(V)

                    val opn = FunStack.top(C)
                    
                    val res = eval_1op(valOf(Int.fromString(m)), opn)

                    val V2 = FunStack.push(Int.toString(res), V1)

                    val C1 = FunStack.pop(C)
                in (V2, M, C1)
                end


        | "SET" =>
                let
                    val m = FunStack.top(V)
                    val V1 = FunStack.pop(V)

                    val x = FunStack.top(V1)
                    val V2 = FunStack.pop(V1)

                    val idx = check(x)
                    val _ = Array.update(M, idx, valOf(Int.fromString(m)))

                    val C1 = FunStack.pop(C)
                
                in (V2, M, C1)
                end

        | "IF" => 
                let
                    
                    val b_val = FunStack.top(V)
                    val V1 = FunStack.pop(V)
                    
                    val C1 = FunStack.pop(C)
                    val (C2,comm_list1) = get_comm_list1(C1, 1)
                    (* val _ = toString(V1, M, C2 ) *)
                    val (C3,comm_list2) = get_comm_list2(C2, 1)

                    val C_comm1 = push_list_on_stack(C3, comm_list1)
                    val C_comm2 = push_list_on_stack(C3, comm_list2)
                in  
                    if (b_val = "1") then (V1, M, C_comm1)
                    else (V1, M, C_comm2)
                end

        | "WHILE" => 
                let
                    val C1 = FunStack.pop(C)
                    val (C2,b) = get_comm_list3(C1, 1)

                    val (C3,c) = get_comm_list4(C2, 1)

                    val C4 = push_list_on_stack(C3, b)

                    val V1 = push_list_on_stack(V, b)
                    val V2 = push_list_on_stack(V1, c)

                in  
                    (V2, M, C4)
                end
        
        | "DO" =>
                let
                    val b_val = FunStack.top(V)
                    val V1 = FunStack.pop(V)

                    val (V2,c) = get_comm_list4(V1, 1)

                    val (V3,b) = get_comm_list3(V2, 1)

                    val C1 = FunStack.pop(C)

                    val C_c = push_list_on_stack(C1, c)
                    val C_cb = push_list_on_stack(C_c, b)
                    val C_cbWH = FunStack.push("WHILE", C_cb)
                    val c_without_endwh = delete_ith(c, List.length c)
                    val C_cbWHc = push_list_on_stack(C_cbWH, c_without_endwh)
                in
                    if (b_val="0") then (V3, M, C1)
                    else (V3, M, C_cbWHc)
                end

        | "READ" => 
                let 
                    val var = FunStack.top(V)
                    val V1 = FunStack.pop(V)

                    val var_val = get_input var
                    val int_val = valOf(Int.fromString(var_val))

                    val _ = if (exist (var, !var_bool_list) = true) 
                    then (if (int_val <> 0 andalso int_val <> 1) then raise BOOLERROR else ())
                    else ()

                    val idx = check(var)
                    val _ = Array.update(M, idx, int_val)

                    val C1 = FunStack.pop(C)
                in (V1,M,C1)
                end

        | "WRITE" => 
                let
                    val m = FunStack.top(V)
                    val V1 = FunStack.pop(V)
                    
                    val _ = TextIO.output(TextIO.stdOut, m^"\n")

                    val C1 = FunStack.pop(C)
                in (V1,M,C1)
                end

        | anyx => let
                    val V1 = FunStack.push(anyx, V)

                    val C1 = FunStack.pop(C)
                in (V1, M, C1)
                end


end
