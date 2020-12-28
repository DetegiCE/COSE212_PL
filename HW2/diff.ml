type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
match exp with
  Sum[] -> Const 0
| Sum[a] -> diff(a,x)
| Times[] -> Const 0
| Times[a] -> diff(a, x)
| Sum (hd::tl) -> Sum([diff(hd, x)] @ [diff(Sum(tl), x)])
| Power (a, 0) -> Const 0
| Power (a, b) -> if a <> x then Const 0 else Times ([Const b] @ [Power(a, b-1)])
| Times (hd::tl) -> Sum([Times([diff(hd,x)]@tl)]@[Times([hd]@[diff(Times tl,x)])])
| Var (a) -> if a = x then Const 1 else Const 0
| Const (a) -> Const 0;;

diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "x");;
diff (Times[Const 2; Var "x"; Var "y"], "x");;
