type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

exception UndefinedError

let rec elemcalc : int -> exp -> int
= fun n exp ->
match exp with 
  X -> n
| INT a -> a
| ADD (a, b) -> (elemcalc n a) + (elemcalc n b)
| SUB (a, b) -> (elemcalc n a) - (elemcalc n b)
| MUL (a, b) -> (elemcalc n a) * (elemcalc n b)
| DIV (a, b) -> (elemcalc n a) / (elemcalc n b)
| SIGMA (a, b, c) -> sigmacalc (elemcalc n a) (elemcalc n b) c
and sigmacalc : int -> int -> exp -> int
= fun a b exp -> if a = b then elemcalc a exp else (if a > b then 0 else (elemcalc a exp) + (sigmacalc (a+1) b exp));;

let rec calculator : exp -> int
= fun exp ->
match exp with
  X -> raise UndefinedError
| INT a -> a
| ADD (a, b) -> (calculator a) + (calculator b)
| SUB (a, b) -> (calculator a) - (calculator b)
| MUL (a, b) -> (calculator a) * (calculator b)
| DIV (a, b) -> (calculator a) / (calculator b)
| SIGMA (a, b, c) -> sigmacalc (calculator a) (calculator b) c;;

calculator (SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)));;
