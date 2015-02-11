

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;


let rec is_mirror t1 t2 =
  match t1, t2 with
  | Empty, Empty -> true
  | Node(_, l1, r1), Node(_, l2, r2) ->
     is_mirror l1 r2 && is_mirror r1 l2
  | _, _ -> false;;


let is_symmetric = function
  | Empty -> true
  | Node(_, t1, t2) -> is_mirror t1 t2;;


let () =
  assert(is_symmetric(Node(1, Empty, Empty)));
  assert(is_symmetric(Node(1, Empty, Node(2, Empty, Empty))) = false);;
