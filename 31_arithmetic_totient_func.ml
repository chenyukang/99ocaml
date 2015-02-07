

let coprime a b =
  let rec gcd a b =
    if b = 0 then a else gcd b (a mod b) in
  gcd a b = 1;;


let phi n =
  let rec count acc d =
    if d < n then
      count (if coprime n d
             then (acc + 1) else acc)
            (d + 1)
    else
      acc in
  if n = 1 then 1 else count 0 1;;



let () =
  assert( phi 10 = 4);
  assert( phi 13 = 12);;
