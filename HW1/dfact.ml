let rec dfact n =
  match n with
    1 -> 1
  | 2 -> 2
  | _ -> n * dfact (n-2);;
  
dfact 7;;
dfact 6;;
