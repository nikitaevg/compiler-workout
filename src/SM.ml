open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let hd_tl = Language.Stmt.hd_tl
let cjmp_sat znz value = if (znz = "nz" && value <> 0) || (znz = "z" && value == 0) then true else false
let rec eval env (st, (s, i, o)) p = if 1 == 0 then (st, (s, i, o)) else
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
                     (tail, (new_state, i, o))
        | LABEL _ -> (st, (s, i, o))
        | _ -> failwith "impossible"
    in match p with
        | x::xs -> (match x with 
            | JMP label -> eval env (st, (s, i, o)) (env#labeled label)
            | CJMP (znz, label) -> let (head, tail) = hd_tl st "Stack is empty on cjmp" in
                if cjmp_sat znz head
                then eval env (tail, (s, i, o)) (env#labeled label)
                else eval env (tail, (s, i, o)) xs
            | _ -> eval env (eval_expr x) xs)
        | _ -> (st, (s, i, o))

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  if (1 == 0) then
  List.fold_left (fun () pr ->
                  Printf.printf "%s\n" (GT.transform(insn) (new @insn[show]) () pr)) () p
  else ();
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o
(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
class labels =
  object (self)
    val label_n = 0
    method get_label = {< label_n = label_n + 1 >}, self#generate_label
    method generate_label = "label" ^ string_of_int label_n
  end

let rec compile_expr e = match e with
    | Language.Expr.Const x -> [CONST x]
    | Language.Expr.Var n -> [LD n]
    | Language.Expr.Binop (op, e1, e2) -> compile_expr e1 @ compile_expr e2 @ [BINOP op]

let rec compile_impl lb p after_label = match p with
    | Language.Stmt.Read name -> ([READ; ST name]), false, lb
    | Language.Stmt.Write expr -> (compile_expr expr @ [WRITE]), false, lb
    | Language.Stmt.Assign (name, expr) -> (compile_expr expr @ [ST name]), false, lb
    | Language.Stmt.Seq (e1, e2) -> let (lb, label) = lb#get_label in
                                    let (prg1, used1, lb) = compile_impl lb e1 label in
                                    let (prg2, used2, lb) = compile_impl lb e2 after_label in
                                    (prg1 @
                                    (if used1 then [LABEL label] else []) @
                                    prg2), used2, lb
    | Language.Stmt.Skip -> [], false, lb
    | Language.Stmt.If (cond, thn, els) ->
        let lb, else_label = lb#get_label in
        let condition = compile_expr cond in
        let (thn_body, used1, lb) = compile_impl lb thn after_label in
        let (els_body, used2, lb) = compile_impl lb els after_label in
        condition @ [CJMP ("z", else_label)] @
        thn_body @ (if used1 then [] else [JMP after_label]) @ [LABEL else_label] @
        els_body @ (if used2 then [] else [JMP after_label])
        , true, lb
    | Language.Stmt.While (cond, body) -> 
        let lb, before_label = lb#get_label in
        let lb, condition_label = lb#get_label in
        let do_body, _, lb = compile_impl lb body condition_label in
        let condition = compile_expr cond in
        [JMP condition_label; LABEL before_label] @
        do_body @ [LABEL condition_label] @ condition @ [CJMP ("nz", before_label)]
        , false, lb
    | Language.Stmt.RepeatUntil (body, cond) -> 
        let (prg, _, lb) = compile_impl lb (Language.Stmt.While (
                                            Language.Stmt.reverse_condition cond, body)) after_label in
        List.tl (prg), false, lb

let rec compile p = let lb, label = (new labels)#get_label in
                    let prg, used, _ = compile_impl lb p label in
                    prg @ (if used then [LABEL label] else [])
