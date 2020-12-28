type nat = ZERO | SUCC of nat

let rec natadd n1 n2 = 
  match n2 with
    ZERO -> n1
  | SUCC(x) -> SUCC (natadd n1 x);;
  
let rec natmul n1 n2 =
  match n2 with
    ZERO -> ZERO
  | SUCC(x) -> natadd (natmul n1 x) n1;;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
let four = SUCC (SUCC (SUCC (SUCC ZERO)));;

natadd two three;;
natmul two three;;

natadd three four;;
natmul three four;;
