(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators
(* Values *)

let rec list_init iter n f =
  if iter < n
  then (f iter) :: (list_init (iter + 1) n f)
  else []

module Value =
  struct

    @type t = Int of int | String of string | Array of t list with show

    let to_int = function 
    | Int n -> n 
    | _ -> failwith "int value expected"

    let to_string = function 
    | String s -> s 
    | _ -> failwith "string value expected"

    let to_array = function
    | Array a -> a
    | _       -> failwith "array value expected"

    let of_int    n = Int    n
    let of_string s = String s
    let of_array  a = Array  a

    let update_string s i x = String.init (String.length s)
                                (fun j -> if j = i then x else s.[j])
    let update_array  a i x = list_init 0 (List.length a)
                                (fun j -> if j = i then x else List.nth a j)

  end
       
let option_to_list x = match x with
    | Some y -> y
    | None -> []

(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> Value.t; l : string -> Value.t; scope : string list}

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
    let enter st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let leave st st' = {st' with g = st.g}

  end

(* Builtins *)
module Builtin =
  struct

    let eval (st, i, o, _) args = function
    | "read"     -> (match i with z::i' -> (st, i', o, Some (Value.of_int z))
                                  | _ -> failwith "Unexpected end of input")
    | "write"    -> let [arg] = args in
                        (st, i, o @ [Value.to_int arg], None)
    | "$elem"    -> let [b; j] = args in
                    (st, i, o, let i = Value.to_int j in
                               Some (match b with
                                     | Value.String s -> Value.of_int @@ Char.code s.[i]
                                     | Value.Array  a -> List.nth a i
                               )
                    )         
    | "$length"  -> let [arg] = args in
                    (st, i, o, Some (Value.of_int (match arg with
                                                   | Value.Array a -> List.length a
                                                   | Value.String s -> String.length s)))
    | "$array"   -> (st, i, o, Some (Value.of_array args))
    | "isArray"  -> let [a] = args in
                    (st, i, o, Some (Value.of_int @@ match a with
                                                     | Value.Array  _ -> 1
                                                     | _ -> 0))
    | "isString" -> let [a] = args in
                    (st, i, o, Some (Value.of_int @@ match a with
                                                     | Value.String _ -> 1
                                                     | _ -> 0))                     
       
  end
    
open Ostap
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant   *) | Const  of int
    (* array              *) | Array  of t list
    (* string             *) | String of string
    (* S-expressions      *) | Sexp   of string * t list
    (* variable           *) | Var    of string
    (* binary operator    *) | Binop  of string * t * t
    (* element extraction *) | Elem   of t * t
    (* length             *) | Length of t 
    (* function call      *) | Call   of string * t list with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* The type of configuration: a state, an input stream, an output stream, an optional value *)
    type config = State.t * int list * int list * Value.t option
                                                            
    (* Expression evaluator

          val eval : env -> config -> t -> int * config


       Takes an environment, a configuration and an expresion, and returns another configuration. The 
       environment supplies the following method

           method definition : env -> string -> int list -> config -> config

       which takes an environment (of the same type), a name of the function, a list of actual parameters and a configuration, 
       an returns a pair: the return value for the call and the resulting configuration
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


    let rec eval env ((s, i, o, _) as config) e = match e with
        | Const x -> (s, i, o, Some (Value.of_int x))
        | Var n -> (s, i, o, Some (State.eval s n))
        | Binop (op, e1, e2) -> let (s, i, o, Some r1) = eval env config e1 in
                                let (s, i, o, Some r2) = eval env (s, i, o, None) e2 in
                                (s, i, o, Some (Value.of_int @@ str_to_op op (Value.to_int r1) (Value.to_int r2)))
        | Call (f, args) ->
                let (s, i, o, args) =
                    List.fold_left (fun (s, i, o, args) arg ->
                                    let (s, i, o, Some res) = eval env (s, i, o, None) arg in
                                    (s, i, o, args @ [res])) (s, i, o, []) args in
                env#definition env f args (s, i, o, None)
        | Array elems ->
                let (s, i, o, elems) = eval_list env config elems in
                (s, i, o, Some (Value.of_array elems))
        | String str -> (s, i, o, Some (Value.of_string str))
        | Elem (e, index) -> let ((s, i, o, Some index) as config) = eval env config index in
                             let (s, i, o, Some e) = eval env config e in
                             let value = match e with
                                        | String s -> Value.of_int @@ Char.code s.[Value.to_int index]
                                        | Array a -> List.nth a (Value.to_int index) in
                             (s, i, o, Some value)
        | Length x -> let (s, i, o, Some x) = eval env config x in
            (s, i, o, Some (match x with
                                | Value.String s -> Value.of_int @@ String.length s
                                | Value.Array a -> Value.of_int @@ List.length a))
    and eval_list env conf xs =
      let vs, (st, i, o, _) =
        List.fold_left
          (fun (acc, conf) x ->
             let (_, _, _, Some v) as conf = eval env conf x in
             v::acc, conf
          )
          ([], conf)
          xs
      in
      (st, i, o, List.rev vs)

                
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
               upper
          );
      upper: ex: primary elems: (-"[" !(parse) -"]")* length: (".length")?
            {
                let with_elems = List.fold_left (fun e ind -> Elem (e, ind)) ex elems in
                match length with
                    | Some _ -> Length with_elems
                    | _ -> with_elems
            };
      primary: n:DECIMAL {Const n}
             | -"(" expr -")"
             | name:IDENT
                p:("(" args:!(Util.list0 parse) ")" {Call (name, args)}
                   | empty {Var name}) {p}
             | "[" elems:!(Util.list0 parse) "]" { Array elems }
             | str:STRING {String (String.sub str 1 (String.length str - 2))}
             | n:CHAR {Const (Char.code n)};
      parse: expr
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* assignment                       *) | Assign of string * Expr.t list * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* return statement                 *) | Return of Expr.t option
    (* loop with a post-condition       *) | RepeatUntil of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment is the same as for expressions
    *)

    let update st x v indexes =
      let rec update a v = function
      | []    -> v
      | i::tl ->
          let i = Value.to_int i in
          (match a with
           | Value.String s when tl = [] ->
                   Value.String (Value.update_string s i (Char.chr @@ Value.to_int v))
           | Value.Array a ->
                   Value.Array (Value.update_array a i (update (List.nth a i) v tl))
          ) 
      in
      State.update x (match indexes with
                        | [] -> v
                        | _ -> update (State.eval st x) v indexes) st
          
    let hd_tl l msg = match l with
        | head::tail -> (head, tail)
        | _ -> failwith(msg)
    let meta x y = match x with
        | Skip -> y
        | _ -> match y with
            | Skip -> x
            | _ -> Seq (x, y)
    let reverse_condition cond = Expr.Binop ("==", cond, Expr.Const 0)
    let rec eval env ((s, i, o, _) as config) k p =
        (*
            Printf.printf "CURR - %s\n" (GT.transform(t) (new @t[show]) () p);
            Printf.printf "KONT - %s\n\n" (GT.transform(t) (new @t[show]) () k);
            *)
        match p with
        | Assign (name, ids, e) -> let ((s, i, o, Some r) as config) = Expr.eval env config e in
                                   let (s, i, o, ids) = Expr.eval_list env config ids in
                                      eval env (update s name r ids, i, o, Some r) Skip k
        | Seq (e1, e2) -> eval env config (meta e2 k) e1
        | Skip -> (match k with
            | Skip -> config
            | _ -> eval env config Skip k)
        | If (cond, thn, els) -> let ((s, i, o, Some cond_value) as c) = Expr.eval env config cond in
                                 if Value.to_int cond_value <> 0 then
                                     eval env c k thn
                                 else
                                     eval env c k els
        | While (cond, body) -> let ((s, i, o, Some cond_value) as c) = Expr.eval env config cond in
                                if Value.to_int cond_value == 0 then eval env c Skip k
                                else eval env c (meta p k) body
        | RepeatUntil (body, cond) -> eval env config (meta (While (reverse_condition cond, body)) k) body
        | Call (name, args) -> eval env (Expr.eval env config (Expr.Call (name, args))) Skip k
        | Return x -> (match x with
            | Some x -> Expr.eval env config x
            | _ -> config)
            
            

    (* Statement parser *)
    ostap (
      stmt: x:IDENT ids: (-"[" !(Expr.parse) -"]")* ":=" e:!(Expr.parse) {Assign (x, ids, e)}
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
            | name:IDENT "(" args:!(Util.list0 Expr.parse) ")"
            {
                Call (name, args)
            }
            | "skip" {Skip}
            | "return" expr:!(Expr.parse)? { Return expr };
      parse: st1:stmt ";" st2:parse {Seq (st1, st2)} | stmt
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (                                      
      arg : IDENT;
      parse: "fun" name:IDENT "(" args:!(Util.list0 arg) ")"
             local:(%"local" !(Util.list arg))?
             "{" body:!(Stmt.parse) "}"
             {
             name, (args, option_to_list(local), body)
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
  let m          = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o, _ =
    Stmt.eval
      (object
         method definition env f args ((st, i, o, r) as conf) =
           try
             let xs, locs, s      =  snd @@ M.find f m in
             let st'              = List.fold_left
                                        (fun st (x, a) -> State.update x a st)
                                    (State.enter st (xs @ locs)) (List.combine xs args) in
             let st'', i', o', r' = Stmt.eval env (st', i, o, r) Stmt.Skip s in
             (State.leave st'' st, i', o', r')
           with Not_found -> Builtin.eval conf args f
       end)
      (State.empty, i, [], None)
      Stmt.Skip
      body
  in
  o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
