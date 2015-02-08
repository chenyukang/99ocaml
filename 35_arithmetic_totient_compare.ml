
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


(* ===================================== *)
let factors n =
  let rec aux d n =
    if n = 1 then [] else
      if n mod d = 0 then
        match aux d (n / d) with
        | (h,n) :: t when h = d -> (h,n+1) :: t
        | l -> (d,1) :: l
      else aux (d+1) n
  in
  aux 2 n;;

let rec pow n p =
  if p < 1 then 1 else
    n * pow n (p-1);;


let phi_imporved n =
  let rec aux acc = function
    | [] -> acc
    | (p, m)::tl -> aux ((p-1) * pow p (m-1) * acc) tl in
  aux 1 (factors n);;


let timeit f a =
  let s = Unix.gettimeofday() in
  (f a);
  let e = Unix.gettimeofday() in
  e -. s;;

  timeit phi_imporved 10090000;;
  timeit phi 10090000;;
