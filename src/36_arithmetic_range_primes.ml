

let is_prime n =
  let n = abs n in
  let rec not_divisor d =
    d * d > n || (n mod d <> 0) && not_divisor(d + 1) in
  n <> 1 && (not_divisor 2);;


let all_primes s e =
  let rec iter acc n =
    if n > e then acc else
      if is_prime n then
        iter (n::acc) (n+1)
      else iter acc (n+1) in
  iter [] s;;

let () =
  assert(List.length (all_primes 2 7920) = 1000);;
