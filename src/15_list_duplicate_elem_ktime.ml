

let rec replicate l n =
  let rec create e n =
    if n == 1 then [e]
    else e::(create e (n - 1)) in
  let rec iter acc = function
    | [] -> []
    | x::tl -> (create x n) @ (replicate tl n) in
  iter [] l;;


replicate ["a";"b";"c"] 3;;
