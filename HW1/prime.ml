let rec pcnt n m =
  if m <= 0 then 0
  else if n mod m = 0 then 1 + pcnt (n) (m-1)
  else pcnt (n) (m-1);;
  
let prime n =
  if pcnt (n) (n) = 2 then true
  else false;;
  
prime 2;;
prime 3;;
prime 4;;
prime 17;;
