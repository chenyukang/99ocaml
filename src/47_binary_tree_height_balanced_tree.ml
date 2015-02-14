type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let add_trees_with left right all =
  let add_right_tree all l =
    List.fold_left (fun a r -> Node('x', l, r) :: a) all right in
  List.fold_left add_right_tree all left;;
