type program = exp (* abstract syntax tree *)
and exp =
  | CONST of int      (* n: integers *)
  | VAR of var        (* x: variables *)
  | ADD of exp * exp
  | SUB of exp * exp
  | READ
  | ISZERO of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
and var = string

type value = Int of int | Bool of bool (* Int 3 / Bool true, Bool false *)
let string_of_value v = match v with Int n -> string_of_int n | Bool b -> string_of_bool b

exception NotImplemented

type env = var -> value
let empty_env : env = fun x -> raise (Failure (x ^ " is not bound in env"))
let extend_env : var -> value -> env -> env
= fun x v e -> fun y -> if x = y then v else e y
let lookup_env : var -> env -> value
= fun x e -> e x

(* slide 5-9 *)

type env = (var * value) list
let empty_env = []
let extend_env x v e = (x,v)::e
let rec lookup_env x e =
    match e with 
    | [] -> raise (Failure (x ^ " is not bound in env"))
    | (y, v)::tl -> if x = y then v else lookup_env x tl