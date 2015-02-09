


let rec extract k list =
  let rec merge e list =
    match list with
    | [] -> []
    | x::tl -> (e::x) :: merge e tl in
  let rec iter n acc list =
    match list with
    | [] -> acc
    | x::tl -> if n = 1 then
                 (iter n ([x]::acc) tl)
               else
                 let ll = iter n [] tl in
                 ll @ (merge x (iter (n-1) acc tl)) in

  iter k [] list;;

let () =
  assert(List.length (extract 2 ["a";"b";"c";"d"]) = 6);;
