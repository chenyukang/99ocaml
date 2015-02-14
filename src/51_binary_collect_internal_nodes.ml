
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec collect_internal = function
  | Empty -> []
  | Node(_, Empty, Empty) -> []
  | Node(v, tl, tr) -> collect_internal tl @ v:: (collect_internal tr);;


let () =
  assert(collect_internal Empty = []);
  assert(collect_internal (Node(1, Node(2, Empty, Empty),
                              Empty)) = [1]);;
