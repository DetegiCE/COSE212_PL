let rec binarize n =
  if n = 0 then [0]
  else if n = 1 then [1]
  else if n mod 2 = 0 then binarize (n/2) @ [0]
  else binarize (n/2) @ [1];;
  
binarize 2;;
binarize 3;;
binarize 8;;
binarize 17;;
binarize 1;;
binarize 0;;
