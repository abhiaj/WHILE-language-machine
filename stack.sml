signature Stack =
sig
    type 'a Stack
    exception EmptyStack
    exception Error of string
    val create: 'a Stack
    val push : 'a * 'a Stack -> 'a Stack
    val pop : 'a Stack -> 'a Stack
    val top : 'a Stack -> 'a
    val empty: 'a Stack -> bool
    val poptop : 'a Stack -> ('a * 'a Stack) option
    val nth : 'a Stack * int -> 'a
    val drop : 'a Stack * int -> 'a Stack
    val depth : 'a Stack -> int
    val app : ('a -> unit) -> 'a Stack -> unit
    val map : ('a -> 'b) -> 'a Stack -> 'b Stack
    val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
    val find : ('a -> bool) -> 'a Stack -> 'a option
    val filter : ('a -> bool) -> 'a Stack -> 'a Stack
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val exists : ('a -> bool) -> 'a Stack -> bool
    val all : ('a -> bool) -> 'a Stack -> bool
    val List2Stack : 'a list -> 'a Stack (* Convert a List into a Stack *)
    val Stack2List: 'a Stack -> 'a list (* Convert a Stack into a List *)
    val toString: ('a -> string) -> 'a Stack -> string
end

structure FunStack :> Stack =
struct
    exception EmptyStack;
    exception Error of string;

    type 'a Stack = 'a list;

    val create = [];

    fun push (x, S) = x::S;

    fun pop [] = raise EmptyStack
        | pop (x::S) = S;

    fun top [] = raise EmptyStack
        | top (x::S) = x;

    fun empty [] = true
        | empty _ = false;
    
    fun poptop [] = NONE
        | poptop (x::S) = SOME (x,S);
    
    fun nth (S, i) = List.nth(S, i);

    fun drop (S, i) = List.drop(S, i);

    fun depth S = List.length(S);

    fun app f S = (List.app f S);

    fun map f S = (List.map f S);

    fun mapPartial f S = (List.mapPartial f S);

    fun find f S = (List.find f S);

    fun filter f S = (List.filter f S);

    fun foldr f init S = (List.foldr f init S)

    fun foldl f init S = (List.foldl f init S)

    fun exists f S = (List.exists f S);

    fun all f S = (List.all f S);

    fun List2Stack L = L;

    fun Stack2List S = S;

    fun toString f [] = ""
        | toString f (x::S) = (f x)^" "^(toString f S);
end