
let rec last_two = function
  | [] | [_] -> None
  | [x;y] -> Some(x, y)
  | _::t -> last_two t
;;


let r = last_two [1; 2; 3];;
let r = last_two [1];;
let r = last_two [];;
