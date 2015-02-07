

type 'a node =
  | One of 'a
  | Many of 'a node list;;

let rec flatten = function
  | [] -> []
  | One x :: tl -> x :: (flatten tl)
  | Many vs :: tl -> (flatten vs) @ (flatten tl);;


let () =
  assert(flatten [One "a"; Many[One "b"; Many[One "c"; One "d"]; One "e" ]] =
           ["a"; "b"; "c"; "d"; "e"]);;
