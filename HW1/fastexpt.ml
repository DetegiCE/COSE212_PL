let rec fastexpt b n =
  if n = 1 then b
  else if n = 0 then 1
  else if n mod 2 = 0 then fastexpt (b) (n/2) * fastexpt (b) (n/2)
  else b * fastexpt (b) (n-1);;
  
fastexpt 2 5;;
fastexpt 2 32;;
fastexpt 3 6;;
fastexpt 10 15;;
