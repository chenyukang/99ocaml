

let slice list s e =
  let rec iter i acc list =
    match list with
      [] -> acc
    | x::tl -> if i < s || i > e
               then iter (i+1) acc tl else
                 iter (i+1) (x::acc) tl in
  List.rev (iter 0 [] list);;

let () =
  assert(slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 =
           ["c"; "d"; "e"; "f"; "g"]);;
