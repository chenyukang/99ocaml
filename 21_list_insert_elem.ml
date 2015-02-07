

let insert_at elem n list =
  let rec iter acc k = function
    | [] -> (if k = n then
               acc @ [elem] else acc)
    | x::tl -> if k = n then
                 (List.rev acc) @ (elem::x::tl) else
                 iter (x::acc) (k + 1) tl in
  iter [] 0 list;;

let () =
  assert(insert_at "alfa" 1 ["a";"b";"c";"d"] =  ["a"; "alfa"; "b"; "c"; "d"]);;
let () =
  assert(insert_at "alfa" 3 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "alfa"; "d"]);;
let () =
  assert(insert_at "alfa" 4 ["a";"b";"c";"d"] = ["d"; "c"; "b"; "a"; "alfa"]);;
