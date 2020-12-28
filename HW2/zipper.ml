let rec zippify : int list -> int list -> int list -> int list
= fun zip l1 l2 ->
match l1, l2 with
  [], [] -> zip
| h1::t1, [] -> zippify (zip@[h1]) t1 l2
| [], h2::t2 -> zippify (zip@[h2]) l1 t2
| h1::t1, h2::t2 -> zippify (zip@[h1]) l2 t1;;

let zipper : int list * int list -> int list
= fun (l1, l2) -> zippify [] l1 l2;;

zipper ([1;3;5],[2;4;6]);;
zipper ([1;3],[2;4;6;8]);;
zipper ([1;3;5;7],[2;4]);;
