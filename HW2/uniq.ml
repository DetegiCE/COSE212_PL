let rec chk : 'a list -> 'a -> bool
= fun lst n ->
match lst with
  [] -> false
| hd::tl -> if hd = n then true else chk tl n;;

let rec uniquify : 'a list -> 'a list -> 'a list
= fun lst1 lst2 -> 
match lst2 with
  [] -> lst1
| hd::tl -> if chk lst1 hd then uniquify lst1 tl else uniquify (lst1@[hd]) tl;;

let uniq : 'a list -> 'a list
= fun lst -> 
  uniquify [] lst;;

uniq [5;6;5;4];;
