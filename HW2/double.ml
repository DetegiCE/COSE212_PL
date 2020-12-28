let double : ('a -> 'a) -> 'a -> 'a
= fun f -> (fun x -> f (f x));;

let inc x = x + 1;;
let mul x = x * 2;;
(double inc) 1;;
(double inc) 2;;
((double double) inc) 0;;
((double (double double)) inc) 5;;
(double mul) 1;;
(double double) mul 2;;
