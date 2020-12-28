let rec check : 'a -> 'a list -> bool
= fun a lst ->
  match lst with
  | [] -> false
  | hd::tl -> if a = hd then true else check a tl

let rec app_aux : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
  | [] -> l2
  | hd::tl -> app_aux tl (if check hd l2 then l2 else l2@[hd])

let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> app_aux l1 (app_aux l2 [])
