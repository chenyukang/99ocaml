

let range s e =
  let diff = if s > e then -1 else 1 in
  let rec iter acc n =
    if n = e then e::acc else
      iter (n::acc) (n + diff) in
  List.rev (iter [] s);;

let () =
  assert(range 4 9 = [4; 5; 6; 7; 8; 9]);;
let () =
  assert(range 9 4 = [9; 8; 7; 6; 5; 4]);;
let () =
  assert(range 1 1 = [1]);;
