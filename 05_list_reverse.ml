
let reverse l =
  let rec rev cur = function
      [] -> cur
    | x::tl -> rev (x::cur) tl in
  rev [] l;;

let r = reverse [1; 2; 3];;
let r = reverse [];;
let r = reverse [1];;
