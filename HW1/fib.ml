let rec fib n =
  match n with
    0 -> 0
  | 1 -> 1
  | _ -> fib (n-1) + fib (n-2);;
  
fib 0;;
fib 1;;
fib 2;;
fib 3;;
fib 4;;
