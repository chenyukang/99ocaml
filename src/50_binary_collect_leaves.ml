

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec collect_leaves = function
  | Empty -> []
  | Node(v, Empty, Empty) -> [v]
  | Node(_, tl, tr) -> collect_leaves tl @ collect_leaves tr;;


let () =
  assert(collect_leaves Empty = []);
  assert(collect_leaves (Node(1, Node(2, Empty, Empty),
                            Empty)) = [2]);;
