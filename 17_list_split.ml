

let split list n =
  let rec iter k acc = function
    | [] -> List.rev acc, []
    | h::tl -> if k = 0 then List.rev acc, tl else
                 iter (k-1) (h::acc) tl in
  iter n [] list;;

let () =
  assert(split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 =
           (["a"; "b"; "c"], ["e"; "f"; "g"; "h"; "i"; "j"]));;
