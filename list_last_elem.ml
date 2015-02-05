
let rec last = function
  | [] -> None
  | [x] -> Some(x)
  | _::tl -> last tl;;


let r = last [1; 2; 3];;
let r = last [];;
