type exp = 
  | V of var
  | P of var * exp
  | C of exp * exp
and var = string

let rec del : var list -> var -> var list
= fun lst x ->
match lst with
  [] -> []
| hd::tl -> if hd = x then del tl x else [hd]@(del tl x);;

let rec checkify : exp -> var list
= fun exp ->
match exp with
  V x -> [x]
| P (x, e) -> del (checkify e) x
| C (a, b) -> (checkify a)@(checkify b);;

let check : exp -> bool
= fun exp -> if checkify exp = [] then true else false;;

check (P("a", V "a"));;
check (P("a", P("a", V "a")));;
check (P("a", P("b", C(V "a", V "b"))));;
check (P("a", C(V "a", P("b", V "a"))));;
check (P("a", V "b"));;
check (P("a", C(V "a", P("b", V "c"))));;
check (P("a", P("b", C(V "a", V "c"))));;
