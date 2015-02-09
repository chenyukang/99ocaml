
let is_prime n =
  let n = abs n in
  let rec not_divisor d =
    d * d > n || (n mod d <> 0) && not_divisor(d + 1) in
  n <> 1 && (not_divisor 2);;

let goldbach n =
  let rec iter d =
    if (is_prime d) && (is_prime (n-d))
    then (d, n - d) else
      iter (d+1) in
  iter 2;;

let () =
  assert(goldbach 28 = (5, 23));;
