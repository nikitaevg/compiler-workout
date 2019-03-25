(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators

(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    let empty_state x = failwith (Printf.sprintf "Undefined variable: %s" x)
    (* Empty state *)
    let empty =
      {g = empty_state; l = empty_state; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let push_scope st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let drop_scope st st' = {st' with g = st.g}

  end
    
open Ostap
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
      
    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let bool_to_int x = if x then 1 else 0
    let int_to_bool x = if x == 0 then false else true

    let boolop_with_ret_to_int op = fun lhs rhs -> bool_to_int (op (int_to_bool lhs) (int_to_bool rhs))
    let boolop_to_int op = fun lhs rhs -> bool_to_int (op lhs rhs)

    let str_to_op op = match op with
        | "+" -> ( + )
        | "-" -> ( - )
        | "*" -> ( * )
        | "/" -> ( / )
        | "%" -> ( mod )
        | "!!" -> boolop_with_ret_to_int ( || )
        | "&&" -> boolop_with_ret_to_int ( && )
        | "==" -> boolop_to_int ( == )
        | "!=" -> boolop_to_int ( != )
        | "<=" -> boolop_to_int ( <= )
        | "<" -> boolop_to_int ( < )
        | ">=" -> boolop_to_int ( >= )
        | ">" -> boolop_to_int ( > )
        | _ -> failwith "unsupported op"


    let rec eval s e = match e with
        | Const x -> x
        | Var n -> State.eval s n
        | Binop (op, e1, e2) -> let r1 = eval s e1 in
                                let r2 = eval s e2 in
                                str_to_op op r1 r2
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    let to_binop op = ostap($(op)), fun x y -> Binop (op, x, y)

    ostap (
      expr:
          !(Util.expr
               (fun x -> x)
               (Array.map (fun (assoc, ops) -> assoc, List.map to_binop ops)
               [|
                  `Lefta, ["!!"];
                  `Lefta, ["&&"];
                  `Nona,  ["<="; ">="; "=="; "!="; ">"; "<";];
                  `Lefta, ["+"; "-"];
                  `Lefta, ["*"; "/"; "%"];
               |])
               primary
          );
      primary: n:DECIMAL {Const n} | x:IDENT {Var x} | -"(" expr -")";
      parse: expr
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | RepeatUntil of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters and a body for given definition
    *)
    let hd_tl l msg = match l with
        | head::tail -> (head, tail)
        | _ -> failwith(msg)
    let reverse_condition cond = Expr.Binop ("==", cond, Expr.Const 0)
    let rec eval env ((s, i, o) as config) p = match p with
        | Read name -> let (head, tail) = hd_tl i "Unexpected end of input" in
                       (State.update name head s, tail, o)
        | Write e -> (s, i, o @ [Expr.eval s e])
        | Assign (name, e) -> (State.update name (Expr.eval s e) s, i, o)
        | Seq (e1, e2) -> let (s1, i1, o1) = eval env config e1 in
                         eval env (s1, i1, o1) e2
        | Skip -> config
        | If (cond, thn, els) -> let cond_value = Expr.eval s cond in
                                 if cond_value <> 0 then
                                     eval env config thn
                                 else
                                     eval env config els
        | While (cond, body) -> let cond_value = Expr.eval s cond in
                                if cond_value == 0 then config
                                else
                                    let c' = eval env config body in
                                    eval env c' (While (cond, body))
        | RepeatUntil (body, cond) -> let c' = eval env config body in
                                      eval env c' (While (reverse_condition cond, body))
        | Call (name, args) ->
            let (arg_names, locals, body) = env#definition name in
            let fun_state = State.push_scope s (arg_names @ locals) in
            let args_values = List.map (Expr.eval s) args in
            let fun_env_w_args =
                List.fold_left (fun s (name, value) -> State.update name value s) fun_state
                               (List.combine arg_names args_values) in
            let (s', i', o') = eval env (fun_env_w_args, i, o) body in
            ((State.drop_scope s' s), i', o')
            
            

    (* Statement parser *)
    ostap (
      stmt: "read" "(" x:IDENT ")" {Read x}
           | "write" "(" e:!(Expr.parse) ")" {Write e}
           | x:IDENT ":=" e:!(Expr.parse) {Assign (x, e)}
           | "if" condition:!(Expr.parse)
                "then" th:!(parse)
                elif:(%"elif" !(Expr.parse) %"then" !(parse))*
                els:(%"else" !(parse))?
                "fi"
                {
                    let else_body = match els with
                        | Some x -> x
                        | _ -> Skip
                    in
                    let t = List.fold_right (fun (cond, body) curr -> If (cond, body, curr)) elif else_body in
                    If (condition, th, t)
                }
            | "while" condition:!(Expr.parse) "do" body:!(parse) "od" { While (condition, body)}
            | "for" init:!(parse) "," cond:!(Expr.parse) "," step:!(parse) "do" body:!(parse) "od"
            {
                Seq(init, While(cond, Seq(body, step)))
            }
            | "repeat" body:!(parse) "until" cond:!(Expr.parse)
            { 
                RepeatUntil (body, cond)
            }
            | name:IDENT "(" args:(!(Expr.parse))* ")"
            {
                Call (name, args)
            }
            | "skip" {Skip};
      parse: st1:stmt ";" st2:parse {Seq (st1, st2)} | stmt
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (                                      
      parse: "fun" name:IDENT "(" args:(IDENT)* ")"
             local:(%"local" (IDENT)*)?
             "{" body:!(Stmt.parse) "}"
             { let local = match local with
                | Some x -> x
                | _ -> [] in
             name, (args, local, body)
             }
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m        = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o  = Stmt.eval (object method definition f = snd @@ M.find f m end) (State.empty, i, []) body in o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
