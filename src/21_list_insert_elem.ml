

(* a complicate version :( *)
let insert_at elem n list =
  let rec iter acc k = function
    | [] -> (if k = n then
               (List.rev acc) @ [elem] else (List.rev acc))
    | x::tl -> if k = n then
                 (List.rev acc) @ (elem::x::tl) else
                 iter (x::acc) (k + 1) tl in
  iter [] 0 list;;

(* simple version *)
let rec insert_at elem n = function
  | [] -> [elem]
  | x::(t as tl) -> if n = 0 then elem::x::tl else
                      x::insert_at elem (n-1) tl;;

let () =
  assert(insert_at "alfa" 1 ["a";"b";"c";"d"] =  ["a"; "alfa"; "b"; "c"; "d"]);;
let () =
  assert(insert_at "alfa" 3 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "alfa"; "d"]);;
let () =
  assert(insert_at "alfa" 4 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "d"; "alfa"]);;
