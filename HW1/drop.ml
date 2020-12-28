let rec drop f lst =
  match lst with
    [] -> []
  | hd::tl -> if (f hd) then (drop f tl) else hd::tl;;
  
drop (fun x -> x mod 2 = 1) [1;3;5;6;7];;
drop (fun x -> x > 5) [1;3;7];;
