
let rec remove_at n = function
  | [] -> []
  | h::tl -> if n = 0 then tl else
               h :: (remove_at (n - 1) tl);;

let () =
  assert(remove_at 1 ["a";"b";"c";"d"] =
           ["a"; "c"; "d"]);;
