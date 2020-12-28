type digit = ZERO | ONE
type bin = digit list

let rec reverse : bin -> bin -> bin
= fun b accu ->
  match b with
  | [] -> accu
  | hd::tl -> reverse tl (hd::accu)

let rec badd_aux : bin -> bin -> bool -> bin
= fun a b carry ->
  match a, b with
  | [], [] -> if carry then [ONE] else []
  | [], a | a, [] -> if carry then badd_aux a [ONE] false else a
  | h1::t1, h2::t2 ->
    begin
      match h1, h2, carry with
      | ZERO, ZERO, false -> ZERO::badd_aux t1 t2 false
      | ONE, ZERO, false | ZERO, ONE, false | ZERO, ZERO, true -> ONE::badd_aux t1 t2 false
      | ONE, ONE, false | ONE, ZERO, true | ZERO, ONE, true -> ZERO::badd_aux t1 t2 true
      | ONE, ONE, true -> ONE::badd_aux t1 t2 true
    end

let badd : bin -> bin -> bin
= fun a b -> reverse (badd_aux (reverse a []) (reverse b []) false) []

let shift : bin -> bin
= fun b ->
  match b with
  | [] -> raise (Failure "Invalid input")
  | [ZERO] -> b
  | b -> b@[ZERO]

let rec bmul_aux : bin -> bin -> bin -> bin
= fun a b accu ->
  match b with
  | [] -> accu
  | ZERO::tl -> bmul_aux a tl (shift accu)
  | ONE::tl -> bmul_aux a tl (badd a (shift accu))

let rec simplify : bin -> bin
= fun b ->
  match b with
  | [] -> [ZERO]
  | ZERO::b -> simplify b
  | b -> b

let bmul : bin -> bin -> bin
= fun a b -> simplify (bmul_aux a b [ZERO])

