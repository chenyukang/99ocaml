
let rec drop list n  =
  let rec iter i = function
    | [] -> []
    | x::tl -> if i == n then
                 iter 1 tl else
                 x::iter (i+1) tl in
  iter 1 list;;

let () =
  assert((drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3) =
           ["a"; "b"; "d"; "e"; "g"; "h"; "j"]);;
