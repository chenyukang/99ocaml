

let is_prime n =
  let n = abs n in
  let rec not_divisor d =
    d * d > n || (n mod d <> 0) && not_divisor(d + 1) in
  n <> 1 && (not_divisor 2);;

let () =
  assert(is_prime 1 == false);
  assert(is_prime 7);
  assert((is_prime 10) = false);;
