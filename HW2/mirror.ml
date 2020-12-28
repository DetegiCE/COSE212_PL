type btree = 
  | Leaf of int
  | Left of btree
  | Right of btree
  | LeftRight of btree * btree

let rec mirror : btree -> btree
= fun tree -> 
match tree with
  Left(x) -> Right(mirror(x))
| Right(x) -> Left(mirror(x))
| LeftRight(x, y) -> LeftRight(mirror(y), mirror(x))
| Leaf(x) -> Leaf(x);;

mirror (Left (LeftRight (Leaf 1, Leaf 2)));;
mirror (Right (LeftRight (Leaf 2, Leaf 1)));;
