let rec forall : ('a -> bool) -> 'a list -> bool
= fun f lst -> 
match lst with 
  [] -> true
| hd::tl -> if f hd then forall f tl else false;;

forall (fun x -> x mod 2 = 0) [1;2;3];;
forall (fun x -> x > 5) [7;8;9];;
