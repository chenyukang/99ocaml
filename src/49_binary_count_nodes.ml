
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec count_leaves = function
  | Empty -> 0
  | Node(_, Empty, Empty) -> 1
  | Node(_, tl, tr) -> count_leaves tl + count_leaves tr;;

let () =
  assert(count_leaves Empty = 0);
  assert(count_leaves (Node(1, Node(2, Empty, Empty),
                           Empty)) = 1);;
