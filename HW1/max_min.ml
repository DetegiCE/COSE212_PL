let rec max (lst: int list) =
  match lst with
    [x] -> x
  | hd::tl -> if hd > (max tl) then hd else (max tl);;
  
let rec min (lst: int list) =
  match lst with
    [x] -> x
  | hd::tl -> if hd < (min tl) then hd else (min tl);;
  
max [1;3;5;2];;
min [1;3;2];;
