

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b);;

let () =
  assert(gcd 7826 20536 == 2);
  assert(gcd 13 27 == 1);;
