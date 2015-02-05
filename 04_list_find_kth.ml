
let rec find_kth k = function
  | [] -> None
  | x::tl -> if k = 1 then Some x else find_kth (k-1) tl;;

let r = find_kth 1 [1; 2];;
let r = find_kth 2 [1; 2];;
let r = find_kth 1 [];;
