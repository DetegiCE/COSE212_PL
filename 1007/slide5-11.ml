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

exception NotImplemented
exception UndefinedSemantics

type value = Int of int | Bool of bool (* Int 3 / Bool true, Bool false *)
let string_of_value v = match v with Int n -> string_of_int n | Bool b -> string_of_bool b

type env = var -> value
let empty_env : env = fun x -> raise (Failure (x ^ " is not bound in env"))
let extend_env : var -> value -> env -> env
= fun x v e -> fun y -> if x = y then v else e y
let lookup_env : var -> env -> value
= fun x e -> e x

(* env |- e -> v *)

let rec eval : env -> exp -> value
= fun env exp -> 
    match exp with
    | CONST n -> Int n
    | VAR x -> lookup_env x env
    | ADD (e1, e2) -> eval_bop env e1 e2 (+)
    | SUB (e1, e2) -> eval_bop env e1 e2 (-)
    | READ -> Int (read_int())
    | ISZERO e ->
      begin
        match eval env e with
        | Int 0 -> Bool true
        | Int _ -> Bool false
        | _ -> raise UndefinedSemantics
      end
    | IF (e1, e2, e3) -> 
      begin
        match eval env e1 with 
        | Bool true -> eval env e2 
        | Bool false -> eval env e3
        | _ -> raise UndefinedSemantics
      end
    | LET (x, e1, e2) ->
      let v1 = eval env e1 in 
        eval (extend_env x v1 env) e2

and eval_bop : env -> exp -> exp -> (int -> int -> int) -> value (* binary operator *)
= fun env e1 e2 bop ->
  let v1 = eval env e1 in
  let v2 = eval env e2 in
    begin
      match v1, v2 with
      | Int n1, Int n2 -> Int (bop n1 n2)
      | _, _ -> raise UndefinedSemantics
    end

(* interpreter *)
let run: program -> value
=fun pgm -> eval empty_env pgm

(*
P -> E
E -> n
   | x
   | E + E
   | E - E
   | iszero E
   | if E then E else E
   | let x = E in E

key concepts: syntax, semantics, interpreter
*)