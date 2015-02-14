
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec insert tree x = match tree with
  | Empty -> Node(x, Empty, Empty)
  | Node(y, tl, tr) ->
     if x = y then tree
     else if x < y then Node(y, insert tl x, tr)
     else Node(y, tl, insert tr x);;

let construct l  = List.fold_left insert Empty l;;

let () =
  assert(construct [3;2;5;7;1] =
           Node (3, Node (2, Node (1, Empty, Empty), Empty),
                 Node (5, Empty, Node (7, Empty, Empty))));;
