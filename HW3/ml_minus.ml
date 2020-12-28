type program = exp
and exp = 
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | NIL
  | CONS of exp * exp      
  | APPEND of exp * exp
  | HEAD of exp
  | TAIL of exp
  | ISNIL of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of (var * var * exp) * (var * var * exp) * exp
  | PROC of var * exp
  | CALL of exp * exp
  | PRINT of exp
  | SEQ of exp * exp
and var = string

type value = 
  | Unit 
  | Int of int 
  | Bool of bool 
  | List of value list
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | MRecProcedure of (var * var * exp) * (var * var * exp) * env
and env = (var * value) list

let rec fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
= fun f accu lst ->
  match lst with
  | [] -> accu
  | hd::tl -> fold_left f (f accu hd) tl

let rec map : ('a -> 'b) -> 'a list -> 'b list
= fun f lst ->
  match lst with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

let rec string_of_value v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | List lst -> "[" ^ fold_left (fun s x -> s ^ ", " ^ x) "" (map string_of_value lst) ^ "]"
  | _ -> "(functional value)"

exception UndefinedSemantics

let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec lookup_env x e = 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " is not bound in env")) 
  | (y,v)::tl -> if x = y then v else lookup_env x tl

let rec eval : exp -> env -> value
= fun exp env ->
  match exp with
  | PRINT e -> (print_endline (string_of_value (eval e env)); Unit)
  | UNIT -> Unit
  | TRUE -> Bool true
  | FALSE -> Bool false
  | CONST n -> Int n
  | VAR x -> lookup_env x env
  | ADD (e1, e2) ->
    begin
      let n1 = eval e1 env in
      let n2 = eval e2 env in
      match n1, n2 with
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> raise UndefinedSemantics
    end
  | SUB (e1, e2) ->
    begin
      let n1 = eval e1 env in
      let n2 = eval e2 env in
      match n1, n2 with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> raise UndefinedSemantics
    end
  | MUL (e1, e2) ->
    begin
      let n1 = eval e1 env in
      let n2 = eval e2 env in
      match n1, n2 with
      | Int n1, Int n2 -> Int (n1 * n2)
      | _ -> raise UndefinedSemantics
    end
  | DIV (e1, e2) ->
    begin
      let n1 = eval e1 env in
      let n2 = eval e2 env in
      match n1, n2 with
      | Int _, Int 0 -> raise UndefinedSemantics
      | Int n1, Int n2 -> Int (n1 / n2)
      | _ -> raise UndefinedSemantics
    end
  | EQUAL (e1, e2) ->
    begin
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match v1, v2 with
      | Int n1, Int n2 -> Bool (n1 = n2)
      | Bool b1, Bool b2 -> Bool (b1 = b2)
      | _ -> raise UndefinedSemantics
    end
  | LESS (e1, e2) ->
    begin
      let n1 = eval e1 env in
      let n2 = eval e2 env in
      match n1, n2 with
      | Int n1, Int n2 -> Bool (n1 < n2)
      | _ -> raise UndefinedSemantics
    end
  | NOT e ->
    begin
      let b = eval e env in
      match b with
      | Bool b -> Bool (not b)
      | _ -> raise UndefinedSemantics
    end
  | NIL -> List []
  | CONS (e1, e2) ->
    begin
      let hd = eval e1 env in
      let tl = eval e2 env in
      match tl with
      | List tl -> List (hd::tl)
      | _ -> raise UndefinedSemantics
    end
  | APPEND (e1, e2) ->
    begin
      let l1 = eval e1 env in
      let l2 = eval e2 env in
      match l1, l2 with
      | List l1, List l2 -> List (l1@l2)
      | _ -> raise UndefinedSemantics
    end
  | HEAD e ->
    begin
      let l = eval e env in
      match l with
      | List (hd::_) -> hd
      | _ -> raise UndefinedSemantics
    end
  | TAIL e ->
    begin
      let l = eval e env in
      match l with
      | List (_::tl) -> List tl
      | _ -> raise UndefinedSemantics
    end
  | ISNIL e ->
    begin
      let l = eval e env in
      match l with
      | List l -> Bool (l = [])
      | _ -> raise UndefinedSemantics
    end
  | IF (e1, e2, e3) ->
    begin
      let cond = eval e1 env in
      match cond with
      | Bool cond -> if cond then eval e2 env else eval e3 env
      | _ -> raise UndefinedSemantics
    end
  | LET (x, e1, e2) -> let v = eval e1 env in eval e2 (extend_env (x, v) env)
  | LETREC (f, x, e1, e2) -> let proc = RecProcedure (f, x, e1, env) in eval e2 (extend_env (f, proc) env)
  | LETMREC ((f, x, e1), (g, y, e2), e3) ->
    let fproc = MRecProcedure ((f, x, e1), (g, y, e2), env) in
    let gproc = MRecProcedure ((g, y, e2), (f, x, e1), env) in
    eval e3 (extend_env (f, fproc) (extend_env (g, gproc) env))
  | PROC (x, e) -> Procedure (x, e, env)
  | CALL (e1, e2) ->
    begin
      let proc = eval e1 env in
      let v = eval e2 env in
      match proc with
      | Procedure (x, e, env) -> eval e (extend_env (x, v) env)
      | RecProcedure (f, x, e, env) -> eval e (extend_env (x, v) (extend_env (f, proc) env))
      | MRecProcedure ((f, x, e1), (g, y, e2), env) ->
        let gproc = MRecProcedure ((g, y, e2), (f, x, e1), env) in
        eval e1 (extend_env (x, v) (extend_env (f, proc) (extend_env (g, gproc) env)))
      | _ -> raise UndefinedSemantics
    end
  | SEQ (e1, e2) -> let _ = eval e1 env in eval e2 env

let runml : program -> value
= fun pgm -> eval pgm empty_env
