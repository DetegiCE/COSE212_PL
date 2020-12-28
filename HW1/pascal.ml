let rec pascal ((n: int), (m: int)) = 
  if n = 0 then 1
  else if m = 0 then 1
  else if n = m then 1
  else (pascal ((n-1), (m-1))) + (pascal ((n-1), (m)));;
  
pascal (0, 0);;
pascal (1, 0);;
pascal (1, 1);;
pascal (2, 1);;
pascal (4, 2);;
