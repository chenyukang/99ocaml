

let reverse l =
  let rec rev cur = function
      [] -> cur
    | x::tl -> rev (x::cur) tl in
  rev [] l;;

let palindrome l =
  let r = reverse l in
  l = r;;

let () = assert(palindrome [1]);;
let () = assert(palindrome [1; 2] = false);;
let () = assert(palindrome [1; 2; 1] = true);;
let () = assert(palindrome [1; 1; 1; 1] = true);;
