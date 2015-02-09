
let factors n =
  let rec aux d n =
    if n = 1 then [] else
      if n mod d = 0 then
        match aux d (n/d) with
        | (h, n)::tl when h = d -> (h, n+1) :: tl
        | tl -> (d, 1)::tl
      else
        aux (d+1) n in
  aux 2 n;;

let () =
  assert(factors 315 =
           [(3, 2); (5, 1); (7, 1)]);;
