
let is_prime n =
  let n = abs n in
  let rec not_divisor d =
    d * d > n || (n mod d <> 0) && not_divisor(d + 1) in
  n <> 1 && (not_divisor 2);;

let factors n =
  let rec iter acc d n =
    if d > n then acc else
      if (is_prime(d) = false) then
        iter acc (d+1) n
      else
        if n mod d = 0 then
          iter (d::acc) d (n / d) else
          iter acc (d+1) n in
  List.rev (iter [] 2 n);;

let () =
  assert(factors 315 = [3; 3; 5; 7]);;
