let rec iter ((n: int), (f: int -> int)) =
  if n = 0 then (fun x -> x)
  else (fun x -> f (iter(n-1, f) x));;
  
iter (5, (fun x -> 2+x)) 0;;
