(* X86 codegeneration interface *)

(* The registers: *)
let regs = [|"%ebx"; "%ecx"; "%esi"; "%edi"; "%eax"; "%edx"; "%ebp"; "%esp"|]

(* We can not freely operate with all register; only 3 by now *)                    
let num_of_regs = Array.length regs - 5

(* We need to know the word size to calculate offsets correctly *)
let word_size = 4

(* We need to distinguish the following operand types: *)
type opnd = 
| R of int     (* hard register                    *)
| S of int     (* a position on the hardware stack *)
| M of string  (* a named memory location          *)
| L of int     (* an immediate operand             *)

(* For convenience we define the following synonyms for the registers: *)         
let ebx = R 0
let ecx = R 1
let esi = R 2
let edi = R 3
let eax = R 4
let edx = R 5
let ebp = R 6
let esp = R 7

(* Now x86 instruction (we do not need all of them): *)
type instr =
(* copies a value from the first to the second operand  *) | Mov   of opnd * opnd
(* makes a binary operation; note, the first operand    *) | Binop of string * opnd * opnd
(* designates x86 operator, not the source language one *)
(* x86 integer division, see instruction set reference  *) | IDiv  of opnd
(* see instruction set reference                        *) | Cltd
(* sets a value from flags; the first operand is the    *) | Set   of string * string
(* suffix, which determines the value being set, the    *)                     
(* the second --- (sub)register name                    *)
(* pushes the operand on the hardware stack             *) | Push  of opnd
(* pops from the hardware stack to the operand          *) | Pop   of opnd
(* call a function by a name                            *) | Call  of string
(* returns from a function                              *) | Ret
(* a label in the code                                  *) | Label of string
(* a conditional jump                                   *) | CJmp  of string * string
(* a non-conditional jump                               *) | Jmp   of string
(* directive                                            *) | Meta  of string
                                                                            
(* Instruction printer *)
let show instr =
  let binop = function
  | "+"   -> "addl"
  | "-"   -> "subl"
  | "*"   -> "imull"
  | "&&"  -> "andl"
  | "!!"  -> "orl" 
  | "^"   -> "xorl"
  | "cmp" -> "cmpl"
  | _     -> failwith "unknown binary operator"
  in
  let opnd = function
  | R i -> regs.(i)
  | S i -> if i >= 0
           then Printf.sprintf "-%d(%%ebp)" ((i+1) * word_size)
           else Printf.sprintf "%d(%%ebp)"  (8+(-i-1) * word_size)
  | M x -> x
  | L i -> Printf.sprintf "$%d" i
  in
  match instr with
  | Cltd               -> "\tcltd"
  | Set   (suf, s)     -> Printf.sprintf "\tset%s\t%s"     suf s
  | IDiv   s1          -> Printf.sprintf "\tidivl\t%s"     (opnd s1)
  | Binop (op, s1, s2) -> Printf.sprintf "\t%s\t%s,\t%s"   (binop op) (opnd s1) (opnd s2)
  | Mov   (s1, s2)     -> Printf.sprintf "\tmovl\t%s,\t%s" (opnd s1) (opnd s2)
  | Push   s           -> Printf.sprintf "\tpushl\t%s"     (opnd s)
  | Pop    s           -> Printf.sprintf "\tpopl\t%s"      (opnd s)
  | Ret                -> "\tret"
  | Call   p           -> Printf.sprintf "\tcall\t%s" p
  | Label  l           -> Printf.sprintf "%s:\n" l
  | Jmp    l           -> Printf.sprintf "\tjmp\t%s" l
  | CJmp  (s , l)      -> Printf.sprintf "\tj%s\t%s" s l
  | Meta   s           -> Printf.sprintf "%s\n" s

(* Opening stack machine to use instructions without fully qualified names *)
open SM

(* Symbolic stack machine evaluator

     compile : env -> prg -> env * instr list

   Take an environment, a stack machine program, and returns a pair --- the updated environment and the list
   of x86 instructions
*)
let mov_mem_mem x y = 
    match x with
        | R _ -> [Mov (x, y)]
        | L _ -> [Mov (x, y)]
        | _ -> match x with
            | R _ -> [Mov (x, y)]
            | _ -> [Mov (x, eax); Mov (eax, y)]

let suffix_for_comparing op = match op with
    | "==" -> "e"
    | "!=" -> "ne"
    | ">" -> "g"
    | ">=" -> "ge"
    | "<" -> "l"
    | "<=" -> "le"
    | _ -> failwith "not cmp operation"

let op_is_cmp op = op = "==" || op = "!=" || op = "<" || op = "<=" || 
                   op = ">" || op = ">="

let binop_mem_mem op x y = 
    match x with
        | R _ | L _ -> y, [op x y]
        | _ -> match x with
            | R _ -> y, [op x y]
            | _ -> edx, [Mov (y, edx); op x edx]

let compile_binop env op =
    let lhs,rhs,env = env#pop2 in
    let a,env = env#allocate in
    env,
    if op = "+" || op = "-" || op = "*" then
        let res, code = binop_mem_mem (fun x y -> Binop (op, x, y)) lhs rhs in
        code @ [Mov(res, a)]
    else if op_is_cmp op then
        let suff = suffix_for_comparing op in
        let _,code = binop_mem_mem (fun x y -> Binop ("cmp", x, y)) lhs rhs in
        [Binop ("^", eax, eax)] @ code @ [Set (suff,"%al"); Mov (eax, a)]
    else if op = "/" || op = "%" then
        let src = if op = "/" then eax else edx in
        [Mov (rhs, eax); Cltd; IDiv lhs; Mov (src, a)]
    else if  op = "!!" then
        let res, code = binop_mem_mem (fun x y -> Binop (op, x, y)) lhs rhs in
         code @ [Binop ("^", eax, eax); Binop ("cmp", L 0, res);
             Set (suffix_for_comparing "!=", "%al"); Mov (eax, a)]
    else if op = "&&" then
         [Binop("^", eax, eax); Binop("^", edx, edx); 
              Binop("cmp", L 0, lhs); Set("ne", "%al"); 
              Binop("cmp", L 0, rhs); Set("ne", "%dl");
              Binop("&&", edx, eax); Mov(eax, a)]
    else
        failwith "unsupported binop"

let rec init_impl cnt = if cnt < 0 then [] else cnt :: init_impl (cnt - 1)
let init cnt = List.rev (init_impl (cnt - 1))
          
let rec compile env p =
    match p with
    | [] -> env, []
    | x::xs ->
        let new_env,code = match x with
            | CONST x -> let a,env' = env#allocate in env',[Mov (L x, a)]
            | READ -> let a,env' = env#allocate in env',[Call "Lread"; Mov (eax, a)]
            | WRITE -> let a,env' = env#pop in env',[Push a; Call "Lwrite"; Pop eax]
            | LD name -> let a,env' = env#allocate in
                         let var = env#loc name in
                         env',(mov_mem_mem var a)
            | ST name -> let a,env' = (env#global name)#pop in
                         let var = env#loc name in
                         env',(mov_mem_mem a var)
            | BINOP op -> compile_binop env op
            | LABEL l -> env,[Label l]
            | JMP label -> env,[Jmp label]
            | CJMP (c, l) -> let a,env = env#pop in env,[Binop ("cmp", L 0, a); CJmp (c, l)]
            | CALL (name, arg_cnt, flag) -> 
                    let (env, args) =
                        List.fold_left (fun (env, args) _ -> 
                            let a, env = env#pop in (env, a::args))
                        (env, []) (init arg_cnt) in
                    let push_args = List.map (fun x -> Push x) args in
                    let (env, get_res) = if flag
                                         then let (a, env) = env#allocate in
                                              env, [Mov (eax, a)]
                                         else
                                              env, [] in
                    env, push_args @ [Call name; Binop ("+", L (arg_cnt * word_size), esp)] @ get_res
            | BEGIN (name, args, locals) -> 
                    let push_regs = List.map (fun x -> Push (R x)) (init num_of_regs) in
                    let prolog = [Push ebp; Mov (esp, ebp)] in
                    let env = env#enter name args locals in
                    env, prolog @ push_regs @ [Binop ("-", M ("$" ^ env#lsize), esp)]
            | END ->
                    let pop_regs = List.map (fun x -> Pop (R x)) (List.rev (init num_of_regs)) in 
                    let meta = [Meta (Printf.sprintf "\t.set %s, %d" env#lsize (env#allocated * word_size))] in
                    let epilogue = [Mov (ebp, esp); Pop ebp; Ret] in
                    env, [Label env#epilogue] @ pop_regs @ epilogue @ meta
            | RET flag ->
                    if flag
                    then let a,env = env#pop in
                         env, [Mov (a, eax); Jmp env#epilogue]
                    else env, [Jmp env#epilogue]
        in let env',code' = compile new_env xs in env',(code @ code') 
(* A set of strings *)           
module S = Set.Make (String)

(* Environment implementation *)
let make_assoc l = List.combine l (init (List.length l))
                     
class env =
  object (self)
    val globals     = S.empty (* a set of global variables         *)
    val stack_slots = 0       (* maximal number of stack positions *)
    val stack       = []      (* symbolic stack                    *)
    val args        = []      (* function arguments                *)
    val locals      = []      (* function local variables          *)
    val fname       = ""      (* function name                     *)
                        
    (* gets a name for a global variable *)
    method loc x =
      try S (- (List.assoc x args)  -  1)
      with Not_found ->  
        try S (List.assoc x locals) with Not_found -> M ("global_" ^ x)
        
    (* allocates a fresh position on a symbolic stack *)
    method allocate =    
      let x, n =
	let rec allocate' = function
	| []                            -> ebx     , 0
	| (S n)::_                      -> S (n+1) , n+1
	| (R n)::_ when n < num_of_regs -> R (n+1) , stack_slots
        | (M _)::s                      -> allocate' s
	| _                             -> S 0     , 1
	in
	allocate' stack
      in
      x, {< stack_slots = max n stack_slots; stack = x::stack >}

    (* pushes an operand to the symbolic stack *)
    method push y = {< stack = y::stack >}

    (* pops one operand from the symbolic stack *)
    method pop = let x::stack' = stack in x, {< stack = stack' >}

    (* pops two operands from the symbolic stack *)
    method pop2 = let x::y::stack' = stack in x, y, {< stack = stack' >}

    (* registers a global variable in the environment *)
    method global x  = {< globals = S.add ("global_" ^ x) globals >}

    (* gets all global variables *)      
    method globals = S.elements globals

    (* gets a number of stack positions allocated *)
    method allocated = stack_slots                                
                                
    (* enters a function *)
    method enter f a l =
      {< stack_slots = List.length l; stack = []; locals = make_assoc l; args = make_assoc a; fname = f >}

    (* returns a label for the epilogue *)
    method epilogue = Printf.sprintf "L%s_epilogue" fname
                                     
    (* returns a name for local size meta-symbol *)
    method lsize = Printf.sprintf "L%s_SIZE" fname

    (* returns a list of live registers *)
    method live_registers =
      List.filter (function R _ -> true | _ -> false) stack
      
  end
  
(* Generates an assembler text for a program: first compiles the program into
   the stack code, then generates x86 assember code, then prints the assembler file
*)
let genasm (ds, stmt) =
  let stmt = Language.Stmt.Seq (stmt, Language.Stmt.Return (Some (Language.Expr.Const 0))) in
  let sm_code = SM.compile (ds, stmt) in
  let env, code =
    compile
      (new env)
      ((LABEL "main") :: (BEGIN ("main", [], [])) :: sm_code)
  in
  let data = Meta "\t.data" :: (List.map (fun s -> Meta (s ^ ":\t.int\t0")) env#globals) in 
  let asm = Buffer.create 1024 in
  List.iter
    (fun i -> Buffer.add_string asm (Printf.sprintf "%s\n" @@ show i))
    (data @ [Meta "\t.text"; Meta "\t.globl\tmain"] @ code);
  Buffer.contents asm

(* Builds a program: generates the assembler file and compiles it with the gcc toolchain *)
let build prog name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (genasm prog);
  close_out outf;
  let inc = try Sys.getenv "RC_RUNTIME" with _ -> "../runtime" in
  Sys.command (Printf.sprintf "gcc -m32 -o %s %s/runtime.o %s.s" name inc name)
 
