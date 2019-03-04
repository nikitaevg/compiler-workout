open GT       
open Language
       
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
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
*)                         
let hd_tl = Language.Stmt.hd_tl
let rec eval (st, (s, i, o)) p = 
    let eval_expr expr = match expr with
        | BINOP op -> (match st with
            | (y::x::xs) -> (Language.Expr.str_to_op op x y :: xs, (s, i, o)) 
            | _ -> failwith "Stack is empty on binop")
        | CONST x -> (x :: st, (s, i, o))
        | READ -> let (head, tail) = hd_tl i "Unexpected end of input" in
                      (head :: st, (s, tail, o))
        | WRITE -> let (head, tail) = hd_tl st "Stack is empty on write" in
                       (tail, (s, i, o @ [head]))
        | LD name -> (s name :: st, (s, i, o))
        | ST name -> let (head, tail) = hd_tl st "Stack is empty on store" in
                     let new_state = Language.Expr.update name head s in
                     (tail, (new_state, i, o)) in
    match p with
        | x::xs -> eval (eval_expr x) xs
        | _ -> (st, (s, i, o))

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile_expr e = match e with
    | Language.Expr.Const x -> [CONST x]
    | Language.Expr.Var n -> [LD n]
    | Language.Expr.Binop (op, e1, e2) -> compile_expr e1 @ compile_expr e2 @ [BINOP op]

let rec compile p = match p with
    | Language.Stmt.Read name -> [READ; ST name]
    | Language.Stmt.Write expr -> compile_expr expr @ [WRITE]
    | Language.Stmt.Assign (name, expr) -> compile_expr expr @ [ST name]
    | Language.Stmt.Seq (e1, e2) -> compile e1 @ compile e2
