
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let add_trees_with left right all =
  let add_right_tree all l =
    List.fold_left (fun a r -> Node('x', l, r) :: a) all right in
  List.fold_left add_right_tree all left;;

let rec cbal_tree n =
  if n = 0 then [Empty]
  else if n mod 2 = 1 then
    let t = cbal_tree(n/2) in
    add_trees_with t t []
  else
    let t1 = cbal_tree (n/2 - 1) in
    let t2 = cbal_tree (n/2) in
    add_trees_with t1 t2 (add_trees_with t2 t1 []);;

let () =
  assert(List.length(cbal_tree 40) = 524288);;
