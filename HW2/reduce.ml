let rec reduce : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
= fun f l1 l2 accu ->
  match l1, l2 with
  | [], [] -> accu
  | h1::t1, h2::t2 -> reduce f t1 t2 (f h1 h2 accu)
  | _ -> raise (Failure "Invalid inputs")
