
let rec length = function
    [] -> 0
  | _::tl -> 1 + length tl;;

let r = length [1];;
let r = length [1; 2; 3];;
let r = length [];;

(* tail recursive version *)
let length list =
  let rec len n = function
    | [] -> n
    | _::tl -> len (n + 1) tl in
  len 0 list;;


let r = length [1];;
let r = length [1; 2; 3];;
let r = length [];;
