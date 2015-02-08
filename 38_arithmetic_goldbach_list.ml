
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

let rec goldbach_list a b =
  if a > b then [] else
    if a mod 2 = 1 then goldbach_list (a+1) b else
      (a, goldbach a)::goldbach_list (a+2) b;;

let goldbach_limit a b limit =
  List.filter (fun (_, (s1, s2)) -> s1 > limit && s2 > limit)
              (goldbach_list a b);;

let () =
  assert(goldbach_list 9 20 =
           [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
            (20, (3, 17))]);
  assert(goldbach_limit 1 2000 50 =
           [(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
            (1928, (61, 1867))]);;
