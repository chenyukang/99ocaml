

let rec duplicate = function
  | [] -> []
  | x::tl -> x::(x::(duplicate tl));;

let () =
  assert(duplicate ["a";"b";"c";"c";"d"] =
           ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]);;
