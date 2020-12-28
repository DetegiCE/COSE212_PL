type btree =
	| Empty
	| Node of int * btree * btree
	
let t1 = Node (1, Empty, Empty);;
let t2 = Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty));;
	
let rec mem n t =
  match t with
    Empty -> false
  | Node(x, y, z) -> if x = n then true else mem n y || mem n z;;
  
mem 1 t1;;
mem 4 t2;;
mem 2 t1;;
