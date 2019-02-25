open GT       
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let hd_tl = Syntax.Stmt.hd_tl
let rec eval (st, (s, i, o)) p = 
    let eval_expr expr = match expr with
        | BINOP op -> (match st with
            | (y::x::xs) -> (Syntax.Expr.str_to_op op x y :: xs, (s, i, o)) 
            | _ -> failwith "Stack is empty on binop")
        | CONST x -> (x :: st, (s, i, o))
        | READ -> let (head, tail) = hd_tl i "Unexpected end of input" in
                      (head :: st, (s, tail, o))
        | WRITE -> let (head, tail) = hd_tl st "Stack is empty on write" in
                       (tail, (s, i, head :: o))
        | LD name -> (s name :: st, (s, i, o))
        | ST name -> let (head, tail) = hd_tl st "Stack is empty on store" in
                     let new_state = Syntax.Expr.update name head s in
                     (tail, (new_state, i, o)) in
    match p with
        | x::xs -> eval (eval_expr x) xs
        | _ -> (st, (s, i, o)) 


(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile_expr e = match e with
    | Syntax.Expr.Const x -> [CONST x]
    | Syntax.Expr.Var n -> [LD n]
    | Syntax.Expr.Binop (op, e1, e2) -> compile_expr e1 @ compile_expr e2 @ [BINOP op]

let rec compile p = match p with
    | Syntax.Stmt.Read name -> [READ; ST name]
    | Syntax.Stmt.Write expr -> compile_expr expr @ [WRITE]
    | Syntax.Stmt.Assign (name, expr) -> compile_expr expr @ [ST name]
    | Syntax.Stmt.Seq (e1, e2) -> compile e1 @ compile e2
