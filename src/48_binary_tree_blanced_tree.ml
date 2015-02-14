
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let add_trees_with left right all =
  let add_right_tree all l =
    List.fold_left (fun a r -> Node('x', l, r) :: a) all right in
  List.fold_left add_right_tree all left;;


let rec hbal_tree h =
  if h = 0 then [Empty]
  else if h = 1 then [Node('x', Empty, Empty)]
  else
    (* [add_trees_with left right trees] is defined in a question above. *)
    let t1 = hbal_tree (h - 1)
    and t2 = hbal_tree (h - 2) in
    add_trees_with t1 t1 (add_trees_with t1 t2 (add_trees_with t2 t1 []));;


let rec min_nodes h =
  if h <= 0 then 0
  else if h = 1 then 1
  else min_nodes (h - 1) + min_nodes (h - 2) + 1;;


let rec max_height = function
  | 0 -> 0
  | n ->
     let h = max_height (n - 1) in
     if max_height (n - min_nodes (h - 1) - 1) = h then h + 1 else h;;

let hbal_tree_nodes n =
  hbal_tree (max_height n);;
