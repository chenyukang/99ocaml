

let coprime a b =
  let rec gcd a b =
    if b = 0 then a else gcd b (a mod b) in
  gcd a b = 1;;

let () =
  assert(coprime 7826 20536 = false);
  assert(coprime 13 27 = true);;
